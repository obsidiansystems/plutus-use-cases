{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
let
deps = obelisk.nixpkgs.thunkSet ./dep;
rhyolite = (import deps.rhyolite { inherit obelisk; });
foldExtensions = lib.foldr lib.composeExtensions (_: _: {});
p = project ./. ({ pkgs, ... }: let haskellLib = pkgs.haskell.lib; in {
  overrides = foldExtensions [
    rhyolite.haskellOverrides
    (self: super: {
      beam-core = haskellLib.doJailbreak super.beam-core; # upstream to reflex-platform; also update aeson to track >=1.5
      beam-migrate = haskellLib.doJailbreak super.beam-migrate; # upstream to reflex-platform; also update aeson to track >=1.5
      beam-automigrate = haskellLib.doJailbreak (self.callHackage "beam-automigrate" "0.1.2.0" {}); # upstream to reflex-platform; also update aeson to track >=1.5
      beam-postgres = haskellLib.doJailbreak super.beam-postgres; # upstream to reflex-platform; also update aeson to track >=1.5
    })
    # overlay for cardano pkgs
    (self: super: {
      cardano-crypto = self.callCabal2nix "cardano-crypto" deps.cardano-crypto {};
      cardano-addresses = haskellLib.doJailbreak (self.callCabal2nixWithOptions "cardano-addresses" (deps.cardano-addresses + "/core") "--no-hpack" {});
      cardano-prelude = self.callCabal2nix "cardano-prelude" (deps.cardano-prelude + "/cardano-prelude") {};
      cardano-prelude-test = self.callCabal2nix "cardano-prelude-test" (deps.cardano-prelude + "/cardano-prelude-test") {};
      cardano-binary = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary" (deps.cardano-base + "/binary") {}) "development";
      cardano-binary-test = haskellLib.enableCabalFlag (self.callCabal2nix "cardano-binary-test" (deps.cardano-base + "/binary/test") {}) "development";
      cardano-slotting = self.callCabal2nix "cardano-slotting" (deps.cardano-base + "/slotting") {};
      cardano-crypto-praos = haskellLib.addPkgconfigDepend (self.callCabal2nix "cardano-crypto-praos" (deps.cardano-base + "/cardano-crypto-praos") {}) pkgs.libsodium; # TODO use libsodium-vrf fork
      cardano-crypto-class = haskellLib.addPkgconfigDepend (self.callCabal2nix "cardano-crypto-class" (deps.cardano-base + "/cardano-crypto-class") {}) pkgs.libsodium; # TODO use libsodium-vrf fork
      strict-containers = self.callCabal2nix "strict-containers" (deps.cardano-base + "/strict-containers") {};
      cardano-crypto-wrapper = haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-wrapper" (deps.cardano-ledger-specs + "/byron/crypto") {});
      # tests fail on some env var not being set
      cardano-ledger-byron = haskellLib.dontCheck (haskellLib.enableCabalFlag (haskellLib.doJailbreak (self.callCabal2nix "cardano-ledger-byron" (deps.cardano-ledger-specs + "/byron/ledger/impl") {})) "development");
      cardano-ledger-core = haskellLib.dontCheck (self.callCabal2nix "cardano-ledger-core" (deps.cardano-ledger-specs + "/cardano-ledger-core") {});
      shelley-spec-non-integral = haskellLib.dontCheck (self.callCabal2nix "shelley-spec-non-integral" (deps.cardano-ledger-specs + "/shelley/chain-and-ledger/dependencies/non-integer") {});
      small-steps = haskellLib.dontCheck (self.callCabal2nix "small-steps" (deps.cardano-ledger-specs + "/semantics/executable-spec") {});
      small-steps-test = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "small-steps-test" (deps.cardano-ledger-specs + "/semantics/small-steps-test") {}));
      byron-spec-chain = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-chain" (deps.cardano-ledger-specs + "/byron/chain/executable-spec") {}));
      byron-spec-ledger = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "byron-spec-ledger" (deps.cardano-ledger-specs + "/byron/ledger/executable-spec") {}));
      cardano-crypto-test = haskellLib.doJailbreak (haskellLib.dontCheck (self.callCabal2nix "cardano-crypto-test" (deps.cardano-ledger-specs + "/byron/crypto/test") {}));
      contra-tracer = haskellLib.dontCheck (self.callCabal2nix "contra-tracer" (deps.iohk-monitoring-framework + "/contra-tracer") {});
      nothunks = self.callHackage "nothunks" "0.1.0.0" {};
      goblins = haskellLib.dontCheck (self.callCabal2nix "goblins" deps.goblins {});
      moo = haskellLib.markUnbroken super.moo;
      gray-code = haskellLib.overrideCabal (haskellLib.markUnbroken super.gray-code) {
        preCompileBuildDriver = "rm Setup.hs";
      };
      typerep-map = haskellLib.markUnbroken super.typerep-map;
      primitive = haskellLib.dontCheck (self.callHackage "primitive" "0.7.1.0" {}); # cardano-crypto-class min bound
      streaming-bytestring = self.callHackage "streaming-bytestring" "0.2.1" {}; # cardano-crypto-class min bound
      # prop_aeson_canonical:     FAIL                            │
      # *** Failed! Falsified (after 12 tests and 4 shrinks):   │
      # JSObject [("\CAN$%OW",JSObject [])]                     │
      # Use --quickcheck-replay=687955 to reproduce.
      canonical-json = haskellLib.dontCheck (haskellLib.doJailbreak (haskellLib.markUnbroken super.canonical-json));
      cborg = haskellLib.dontCheck super.cborg; # tests don't build for base16-bytestring >=1
      base16-bytestring = self.callHackage "base16-bytestring" "1.0.0.0" {}; # for cardano-prelude
      unordered-containers = self.callHackage "unordered-containers" "0.2.12.0" {}; # for cardano-addresses
      protolude = self.callHackage "protolude" "0.3.0" {}; # for cardano-prelude
      formatting = self.callHackage "formatting" "7.1.0" {};
      fmt = self.callCabal2nix "fmt" deps.fmt {};
      bech32 = haskellLib.dontCheck (self.callCabal2nix "bech32" (deps.bech32 + "/bech32") {}); # 1.1.1 ; tests rely on bech32 executable
      bech32-th = self.callCabal2nix "bech32-th" (deps.bech32 + "/bech32-th") {}; #1.1.1
    })
  ];
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
});
in builtins.removeAttrs p ["ios" "android"]

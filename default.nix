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

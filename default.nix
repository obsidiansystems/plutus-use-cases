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
p = project ./. ({ pkgs, ... }: let haskellLib = pkgs.haskell.lib; in {
  overrides = pkgs.lib.composeExtensions
    rhyolite.haskellOverrides
    (self: super: {
      beam-core = haskellLib.doJailbreak super.beam-core; # upstream to reflex-platform; also update aeson to track >=1.5
      beam-migrate = haskellLib.doJailbreak super.beam-migrate; # upstream to reflex-platform; also update aeson to track >=1.5
      beam-automigrate = haskellLib.doJailbreak (self.callHackage "beam-automigrate" "0.1.2.0" {}); # upstream to reflex-platform; also update aeson to track >=1.5
      beam-postgres = haskellLib.doJailbreak super.beam-postgres; # upstream to reflex-platform; also update aeson to track >=1.5
    });
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
});
in builtins.removeAttrs p ["ios" "android"]

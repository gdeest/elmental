{
  description = "A Nix flake for the Haskell library 'elmental'";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mkElmDerivation.url = github:jeslie0/mkElmDerivation;
  };

  outputs = { self, nixpkgs, flake-utils, mkElmDerivation }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
            overlays = [ mkElmDerivation.overlays.mkElmDerivation ];
            inherit system;
        };

        hsPkgs = pkgs.haskellPackages;
        elmentalPkg = (hsPkgs.callCabal2nix "elmental" (./.) {}).overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [ pkgs.elmPackages.elm ];
          checkPhase = ''
            export HOME=$(mktemp -d)
            ${oldAttrs.checkPhase}
          '';
        });
      in
      {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [elmentalPkg];
          withHoogle = true;
          buildInputs = [
            pkgs.cabal-install
            pkgs.elm2nix
            pkgs.elmPackages.elm
          ];
        };

        packages.elmental = elmentalPkg;
        packages.test-app = pkgs.mkElmDerivation {
          pname = "elm-app";
          version = "0.1.0";
          src = ./test-app;
          outputJavaScript = false;
        };
      }
    );
}

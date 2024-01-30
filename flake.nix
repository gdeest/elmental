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

        elmentalPkg = (hsPkgs.callCabal2nix "elmental" (./.) { }).overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [ pkgs.elmPackages.elm ];
          checkPhase = ''
            export HOME=$(mktemp -d)
            ${oldAttrs.checkPhase}
          '';
        });

        generated-code = pkgs.runCommand "generated-code" { } ''
          mkdir -p $out
          cd $out
          ${elmentalPkg}/bin/generate-test-app-code
        '';

      in
      {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ elmentalPkg ];
          withHoogle = true;
          buildInputs = [
            pkgs.cabal-install
            pkgs.elm2nix
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.haskellPackages.fourmolu
            pkgs.nixpkgs-fmt
            pkgs.treefmt
          ];
        };

        packages.elmental = elmentalPkg;
        packages.genCode = generated-code;
        packages.test-app = pkgs.mkElmDerivation {
          pname = "elm-app";
          version = "0.1.0";
          src = pkgs.symlinkJoin {
            name = "test-app-src";
            paths = [
              ./test-app
              generated-code
            ];
          };
          outputJavaScript = false;
        };
      }
    );
}

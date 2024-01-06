{
  description = "A Nix flake for the Haskell library 'elmental'";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
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
        packages.test-app = pkgs.callPackage ./test-app {};
      }
    );
}

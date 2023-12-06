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
          overlays = [];
        };
        hsPkgs = pkgs.haskellPackages;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with hsPkgs; [
            ghc
            (hsPkgs.callCabal2nix "elmental" (./.) {})
          ];
        };
      }
    );
}

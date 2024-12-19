{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        projectName = "choroszcz-escape";
        pkgs = import nixpkgs { inherit system; };
        haskellPkgs = pkgs.haskellPackages;
      in {
        packages.prolog = pkgs.writeShellApplication {
          name = projectName + "-pl";
          runtimeInputs = [ pkgs.swi-prolog ];
          text = ''
            cd "${./.}/prolog"
            ./run.sh
          '';
        };

        packages.haskell = let
          haskellProjectName = projectName + "-hs";
        in
          haskellPkgs.callCabal2nix haskellProjectName ./haskell {};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # prolog
            swi-prolog

            # haskell
            haskell-language-server
            ghcid
            cabal-install
          ];
        };
      });
}

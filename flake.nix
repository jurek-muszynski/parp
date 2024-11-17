{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        packages.prolog = pkgs.writeShellApplication {
          name = "choroszcz-escape-pl";
          runtimeInputs = [ pkgs.swi-prolog ];
          text = ''
            cd "${./.}/prolog"
            ./run.sh
          '';
        };

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            swi-prolog
          ];
        };
      });
}

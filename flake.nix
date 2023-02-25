{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        pkgs = import nixpkgs {
          inherit system;
        };

        compiler = "ghc943";

        hPkgs =
          pkgs.haskell.packages.${compiler}; # must match `stack.yaml` resolver

        # wrap Stack to work with Nix integration.
        # - no-nix:        # don't use stack's way of integrating Nix
        # --system-ghc     # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        devPkgs = [
          hPkgs.ghc

          stack-wrapped

          pkgs.lmdb
          pkgs.zlib
        ];

      in

      {

        devShells.default = pkgs.mkShell {
          buildInputs = devPkgs;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devPkgs;
        };

      });
}

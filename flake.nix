{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        compiler = "ghc948";

        overlays = [ haskellNix.overlay
          (final: prev: {
            pallas =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = compiler;
              };
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

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

          # uncomment to get working HLS (warning: builds the world from
          # source, you will need lots of time and memory).
          # hPkgs.haskell-language-server

          # Doesn't need to strictly match GHC version, and builds from
          # source (cache miss) if I use `hPkgs.ghcid`
          pkgs.haskellPackages.ghcid
          pkgs.haskellPackages.stylish-haskell

          stack-wrapped

          pkgs.gmp
          pkgs.pkg-config
          pkgs.lmdb
          pkgs.zlib
        ];

        flake = pkgs.pallas.flake {};

      in

      {

        devShells.default = pkgs.mkShell {
          buildInputs = devPkgs;
          nativeBuildInputs = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-live
            pkgs.nodePackages.uglify-js
          ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devPkgs;
        };

        packages = rec {
          pallas = flake.packages."plunder:exe:pallas";
          plock = flake.packages."plunder:exe:plock";
          rex = flake.packages."plunder:exe:rex";
          default = pallas;
          hnix-roots = pkgs.plunder.roots;
        };

      });
}

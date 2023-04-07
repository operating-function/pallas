{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = [
        pkgs.elmPackages.elm
        pkgs.elmPackages.elm-live
        pkgs.nodePackages.uglify-js
    ];
}

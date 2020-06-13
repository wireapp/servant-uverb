let
  channels_ = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  channels  = builtins.mapAttrs (k: v: import (builtins.fetchGit v) {
  }) channels_;
  pkgs = channels."nixpkgs-unstable";
in pkgs.mkShell {
  name = "shell-file";
  buildInputs = [
    pkgs.haskell.compiler.ghc864
    pkgs.cabal-install
    pkgs.pkg-config
    pkgs.ghcid
  ];
  nativeBuildInputs = [
    pkgs.zlib
    pkgs.gmp
    pkgs.ncurses
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.gmp}/lib:${pkgs.zlib}/lib:${pkgs.ncurses}/lib
  '';
}

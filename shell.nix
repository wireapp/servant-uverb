let pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  name = "shell-file";
  buildInputs = [
    pkgs.haskell.compiler.ghc864
    pkgs.cabal-install
    pkgs.pkg-config
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


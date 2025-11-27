{ pkgs ? import <nixpkgs> {} }:

let
  # GHC version compatible with lts-24.21 (GHC 9.10.3)
  haskellPackages = pkgs.haskell.packages.ghc910;
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    # Stack for building the project
    stack

    # GHC and Haskell tooling
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server

    # Build tools (system libraries that GHC needs)
    gmp
    zlib
    ncurses
  ];

  # Set up environment variables
  shellHook = ''
    echo "AdventOfCode development environment"
    echo "Stack version: $(stack --version)"
    echo "GHC version: $(ghc --version)"
  '';
}

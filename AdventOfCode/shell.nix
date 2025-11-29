{
  nixpkgs ? import <nixpkgs> { },
  compiler ? "default",
  doBenchmark ? false,
}:

let

  inherit (nixpkgs) pkgs;

  f =
    {
      mkDerivation,
      aeson,
      base,
      bytestring,
      containers,
      cryptohash-md5,
      ghc,
      hpack,
      lib,
      megaparsec,
      mtl,
      parallel,
      random,
      regex-tdfa,
      scientific,
      split,
      tasty,
      tasty-hunit,
      text,
      vector,
    }:
    mkDerivation {
      pname = "AdventOfCode";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        base
        bytestring
        containers
        cryptohash-md5
        ghc
        megaparsec
        mtl
        parallel
        random
        regex-tdfa
        scientific
        split
        tasty
        tasty-hunit
        text
        vector
      ];
      libraryToolDepends = [
        hpack
        # BROKEN haskellPackages.cabal2ghci
        haskellPackages.cabal2nix
        haskellPackages.fourmolu
        haskellPackages.hlint
        haskellPackages.ghcid
      ];
      executableHaskellDepends = [
        aeson
        base
        bytestring
        containers
        cryptohash-md5
        ghc
        megaparsec
        mtl
        parallel
        random
        regex-tdfa
        scientific
        split
        tasty
        tasty-hunit
        text
        vector
      ];
      testHaskellDepends = [
        aeson
        base
        bytestring
        containers
        cryptohash-md5
        ghc
        megaparsec
        mtl
        parallel
        random
        regex-tdfa
        scientific
        split
        tasty
        tasty-hunit
        text
        vector
      ];
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
      doBenchmark = true;
      prePatch = "hpack";
      license = lib.licenses.bsd3;
      mainProgram = "AdventOfCode-exe";
      maintainers = [ lib.maintainers.benkio ];
    };

  haskellPackages =
    if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f { });

in

if pkgs.lib.inNixShell then drv.env else drv

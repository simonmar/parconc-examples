{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, abstract-par, accelerate, alex, array, async
      , base, binary, bytestring, containers, deepseq, directory
      , distributed-process, distributed-process-simplelocalnet
      , distributed-static, filepath, happy, http-conduit, lib, monad-par
      , network, network-uri, normaldistribution, parallel, random, repa
      , stm, template-haskell, time, transformers, utf8-string, vector
      , xml
      }:
      mkDerivation {
        pname = "parconc-examples";
        version = "0.4.8";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          abstract-par accelerate array async base binary bytestring
          containers deepseq directory distributed-process
          distributed-process-simplelocalnet distributed-static filepath
          http-conduit monad-par network network-uri normaldistribution
          parallel random repa stm template-haskell time transformers
          utf8-string vector xml
        ];
        executableToolDepends = [ alex happy ];
        homepage = "http://github.com/simonmar/parconc-examples";
        description = "Examples to accompany the book \"Parallel and Concurrent Programming in Haskell\"";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

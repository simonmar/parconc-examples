let 
  nixpkgs = import (builtins.fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/59b8d9cf24e9fcf10341a0923c9bdca088dca8c8.tar.gz";}) {};
 
  modern-nixpkgs = import <unstable> {};
  compiler = "ghc884";

  hpkgs = nixpkgs.haskell.packages.${compiler};

  # ghc-events-pkgs = import (builtins.fetchTarball {
  #                     url = "https://github.com/NixOS/nixpkgs/archive/7cc52df39e93d427960646a1797362048fa46011.tar.gz";
  #                   }) {};

  parconc-pkgs = import ./parconc-examples.nix {inherit nixpkgs compiler; };

in 
  hpkgs.shellFor rec {
    # inherit parconc-pkgs;
    name = "parconc-examples";
    packages = _: [];
    nativeBuildInputs = with hpkgs; [ 
      modern-nixpkgs.stack
      modern-nixpkgs.cabal-install
      haskell-language-server
      # ghc-events
      # hvega 
      modern-nixpkgs.haskellPackages.eventlog2html
      # (final: prev: {final.hpkgs.eventlog2html =  prev.hpkgs.eventlog2html.override { hvega = prev.hpkgs.hvega; }; })
      nixpkgs.pkg-config # Required to build some packages
      nixpkgs.zlib # Required to build some packages
      parallel
      modern-nixpkgs.haskellPackages.implicit-hie
      threadscope
    ]; 

    LD_LIBRARY_PATH = modern-nixpkgs.lib.makeLibraryPath (with nixpkgs; [ pkg-config zlib ]); 
}

let 
  nixpkgs = import (builtins.fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/a5c9c6373aa35597cd5a17bc5c013ed0ca462cf0.tar.gz";}) {};
 
  modern-nixpkgs = import <nixpkgs> {};
  compiler = "ghc822";

  hpkgs = nixpkgs.haskell.packages.${compiler};
  # hs = import ./parconc-examples.nix;

  # ghc-events-pkgs = import (builtins.fetchTarball {
  #                     url = "https://github.com/NixOS/nixpkgs/archive/7cc52df39e93d427960646a1797362048fa46011.tar.gz";
  #                   }) {};

  parconc-pkgs = import ./parconc-examples.nix {inherit nixpkgs compiler; doBenchmark = false; };

in 
  hpkgs.shellFor {
    # inherit parconc-pkgs;
    name = "parconc-examples";
    packages = _: [];
    nativeBuildInputs = with hpkgs; [ 
      modern-nixpkgs.stack
      modern-nixpkgs.cabal-install
      # (nixpkgs.haskell-language-server.override { supportedGhcVersions = [ compiler ];})
      # ghc-events
      # hvega 
      # eventlog2html
      # (final: prev: {final.hpkgs.eventlog2html =  prev.hpkgs.eventlog2html.override { hvega = prev.hpkgs.hvega; }; })
      nixpkgs.pkgconfig # Required to build some packages
      nixpkgs.zlib # Required to build some packages
      parallel
      modern-nixpkgs.haskellPackages.implicit-hie
      modern-nixpkgs.haskellPackages.threadscope
  ]; # ++ hs.hackages hpkgs;
}

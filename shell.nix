let 
  nixpkgs = import (builtins.fetchTarball {
            url = "https://github.com/NixOS/nixpkgs/archive/5c79b3dda06744a55869cae2cba6873fbbd64394.tar.gz";}) {}; 

  hpkgs = nixpkgs.haskell.packages."ghc884";
  # hs = import ./parconc-examples.nix;
in 
  hpkgs.shellFor {
    name = "parconc-examples";
    packages = _: [];
    nativeBuildInputs = with hpkgs; [ 
      stack
      cabal-install
      haskell-language-server
      nixpkgs.pkg-config
      nixpkgs.zlib
      parallel
      implicit-hie
      threadscope
  ]; # ++ hs.hackages hpkgs;
}

# Version 0.4.3

* Fix build with GHC 7.8

# Version 0.4.2

* Minor fixes to the .cabal file

# Version 0.4.1

* Make findpar5.hs compile, and add it to the .cabal file

# Version 0.4

* Add `stack.yaml`, builds using LTS 4.2 (GHC 7.10.3)
* Various upper bound updates,including `network-2.6`
* Add `README.md`, with build instructions for Stack and Cabal
* Fix `.cabal` problems found by stack
* Build `tmvar.hs`, `windowman.hs` by importing them into a dummy `Main`
  module in `miscmodules.hs`.
* Removed generated `Parse.hs` and `Lex.hs`

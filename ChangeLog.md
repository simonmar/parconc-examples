# Verison 0.4.8

* Builds with GHC 8.6.x and 8.8.x
* Requires cabal-install 2.2 or later
* Bugfix to the chat example, prevent two users simultaneously kicking
  each other

# Version 0.4.7

* Builds with GHC 8.2.x and 8.4.x

# Version 0.4.6

* Builds with GHC 7.10 and GHC 8.0.2
* Test with cabal new-build, and addit to the instructions

# Version 0.4.5

* Fix build with GHC 7.8

# Version 0.4.4

* Use http-conduit instead of HTTP (fixes problems with wikipedia URL examples)

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

# (c) Simon Marlow 2011, see the file LICENSE for copying terms.
#
# Sample code to accompany 
#   "Parallel and Concurrent Programming in Haskell"
#
# Requisites:
#    - Haskell Platform
#    - monad-par package (cabal install monad-par)
#
# To build all samples, just type "make".

SAMPLES = \
  sudoku1 \
  sudoku2 \
  sudoku3 \
  sudoku4 \
  fork \
  geturls \
  geturlscancel \
  geturlsstm \
  server \
  bingtranslator \
  bingtranslatorconc \
  kmeans/kmeans \
  parinfer/parinfer

all : $(SAMPLES)

GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts -eventlog

% : %.hs
	$(GHC) $(GHC_OPTS) --make  $< -o $@

kmeans/kmeans : kmeans/kmeans.hs
	$(GHC) $(GHC_OPTS) -ikmeans --make  $< -o $@

parinfer/parinfer : parinfer/parinfer.hs
	$(GHC) $(GHC_OPTS) -iparinfer --make  $< -o $@

clean :
	rm *.o *.hi *.eventlog $(SAMPLES)

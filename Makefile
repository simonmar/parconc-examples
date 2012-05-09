# (c) Simon Marlow 2011, see the file LICENSE for copying terms.
#
# Sample code to accompany 
#   "Parallel and Concurrent Programming in Haskell"
#
# Requisites:
#    - Haskell Platform
#    - cabal install xml
#    - for monad-par examples: cabal install monad-par 
#    - for remote examples: cabal install remote derive
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
  geturlsfirst \
  geturlsstm \
  server \
  bingtranslator \
  bingtranslatorconc \
  kmeans/kmeans \
  parinfer/parinfer \
  chat/chat \
  remote-ping/ping \
  remote-ping-multi/ping-multi \
  remote-chat/chat

# -----------------------------------------------------------------------------

all : $(SAMPLES)

GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts -eventlog

% : %.hs
	$(GHC) $(GHC_OPTS) --make  $< -o $@

kmeans/kmeans : kmeans/kmeans.hs
	$(GHC) $(GHC_OPTS) -ikmeans --make  $< -o $@

parinfer/parinfer : parinfer/parinfer.hs
	$(GHC) $(GHC_OPTS) -iparinfer --make  $< -o $@

chat/chat : chat/Main.hs
	$(GHC) $(GHC_OPTS) -ichat --make $< -o $@

remote-ping/ping : remote-ping/ping.hs
	$(GHC) $(GHC_OPTS) -iremote-ping --make $< -o $@

remote-ping-multi/ping-multi : remote-ping-multi/ping-multi.hs
	$(GHC) $(GHC_OPTS) -iremote-ping-multi --make $< -o $@

remote-chat/chat : remote-chat/chat.hs
	$(GHC) $(GHC_OPTS) -iremote-chat --make $< -o $@

clean :
	rm *.o *.hi *.eventlog $(SAMPLES)

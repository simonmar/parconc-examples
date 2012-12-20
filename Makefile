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
  sudoku5 \
  fork \
  reminders \
  reminders2 \
  mvar1 \
  mvar2 \
  mvar3 \
  logger \
  geturls1 \
  geturls2 \
  geturls3 \
  geturls4 \
  geturls5 \
  geturls6 \
  geturls7 \
  geturls8 \
  geturls9 \
  chan \
  chan2 \
  chan3 \
  deadlock1 \
  deadlock2 \
  threadperf1 \
  threadperf2 \
  geturlscancel \
  geturlscancel2 \
  timeout \
  timeout2 \
  catch-mask \
  catch-mask2 \
  windowman \
  tmvar \
  geturlsfirst \
  TList \
  TQueue \
  geturlsstm \
  Async \
  server \
  server2 \
  bingtranslator \
  bingtranslatorconc \
  kmeans/kmeans \
  parinfer/parinfer \
  chat \
  findseq \
  findpar \
  findpar2 \
  findpar3 \
  distrib-ping/ping \
  distrib-ping/ping-multi \
  distrib-ping/ping-tc \
  distrib-ping/ping-tc-merge \
  distrib-ping/ping-tc-notify \
  distrib-ping/ping-fail \
  distrib-chat/chat \
  distrib-chat/chat-fail 

#  distrib-db/db \
#  distrib-db/db2 \
#  distrib-db/db4

# This one needs a bigger stack due to replicateM not being tail-recursive
threadperf2_HC_OPTS = -with-rtsopts=-K32m

# -----------------------------------------------------------------------------

all : $(SAMPLES)

GHC = ghc -package-conf $$HOME/ext/code/distributed-process/cabal-dev/packages-7.4.1.conf
GHC_OPTS = -O2 -threaded -rtsopts -eventlog

% : %.hs
	$(GHC) $(GHC_OPTS) --make $($*_HC_OPTS) $< -o $@

kmeans/kmeans : kmeans/kmeans.hs
	$(GHC) $(GHC_OPTS) -ikmeans --make  $< -o $@

parinfer/parinfer : parinfer/parinfer.hs
	$(GHC) $(GHC_OPTS) -iparinfer --make  $< -o $@

chat/chat : chat/Main.hs
	$(GHC) $(GHC_OPTS) -ichat --make $< -o $@

distrib-ping/ping : distrib-ping/ping.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-ping/ping-multi : distrib-ping/ping-multi.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-ping/ping-tc : distrib-ping/ping-tc.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-ping/ping-tc-merge : distrib-ping/ping-tc-merge.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-ping/ping-tc-notify : distrib-ping/ping-tc-notify.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-ping/ping-fail : distrib-ping/ping-fail.hs
	$(GHC) $(GHC_OPTS) -idistrib-ping --make $< -o $@

distrib-chat/chat : distrib-chat/chat.hs
	$(GHC) $(GHC_OPTS) -idistrib-chat --make $< -o $@

distrib-chat/chat-noslave : distrib-chat/chat-noslave.hs
	$(GHC) $(GHC_OPTS) -idistrib-chat --make $< -o $@

distrib-db/db : distrib-db/db.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

distrib-db/db2 : distrib-db/db2.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

distrib-db/db4 : distrib-db/db4.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

clean :
	rm -f *.o *.hi */*.o */*.hi *.eventlog $(SAMPLES)

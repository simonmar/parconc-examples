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
  other/bingtranslator \
  other/bingtranslatorconc \
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
  distrib-chat/chat-noslave \
  distrib-db/db

#  distrib-db/db2 \

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

distrib-db/db : distrib-db/db.hs distrib-db/Database.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

distrib-db/db2 : distrib-db/db2.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

distrib-db/db4 : distrib-db/db4.hs
	$(GHC) $(GHC_OPTS) -idistrib-db --make $< -o $@

clean :
	rm -f *.o *.hi */*.o */*.hi *.eventlog $(SAMPLES)


setup-kmeans:
	cd kmeans && ../dist/build/GenSamples/GenSamples 5 50000 100000 1010

# Test as much as we can
test:
	cabal build
#	bingtranslator/bingtranslator
#	bingtranslatorconc/bingtranslatorconc
	./dist/build/catch-mask2/catch-mask2
	./dist/build/catch-mask/catch-mask
	./dist/build/chan2/chan2
	./dist/build/chan3/chan3
	./dist/build/chan/chan
#	./dist/build/chat/chat
	if ! ./dist/build/deadlock1/deadlock1; then true; else false; fi
	./dist/build/deadlock2/deadlock2
#	./dist/build/distrib-chat/distrib-chat
#	./dist/build/distrib-chat-noslave/distrib-chat-noslave
	./dist/build/distrib-db/distrib-db
#	./dist/build/findpar2/findpar2
#	./dist/build/findpar3/findpar3
#	./dist/build/findpar/findpar
#	./dist/build/findseq/findseq
#	./dist/build/fork/fork
	./dist/build/fwsparse/fwsparse 100 80
	./dist/build/fwsparse1/fwsparse1 1000 800 +RTS -N2
	./dist/build/geturls1/geturls1
	./dist/build/geturls2/geturls2
	./dist/build/geturls3/geturls3
	./dist/build/geturls4/geturls4
	./dist/build/geturls5/geturls5
	./dist/build/geturls6/geturls6
	./dist/build/geturls7/geturls7
	./dist/build/geturls8/geturls8
	./dist/build/geturls9/geturls9
	./dist/build/geturlscancel2/geturlscancel2
	./dist/build/geturlscancel/geturlscancel
	./dist/build/geturlsfirst/geturlsfirst
	./dist/build/geturlsstm/geturlsstm
	cd kmeans && ../dist/build/kmeans/kmeans seq
	cd kmeans && ../dist/build/kmeans/kmeans strat 64 +RTS -N2
	./dist/build/logger/logger
	./dist/build/modifytwo/modifytwo
	./dist/build/mvar1/mvar1
	./dist/build/mvar2/mvar2
	if ! ./dist/build/mvar3/mvar3; then true; else false; fi
	./dist/build/ping-fail/ping-fail
	./dist/build/ping-multi/ping-multi
	./dist/build/ping/ping
	./dist/build/ping-tc-merge/ping-tc-merge
	./dist/build/ping-tc-notify/ping-tc-notify
	./dist/build/ping-tc/ping-tc
#	./dist/build/reminders2/reminders2
#	./dist/build/reminders/reminders
	echo testing | ./dist/build/rsa/rsa encrypt - | ./dist/build/rsa/rsa decrypt -
	echo testing | ./dist/build/rsa1/rsa1 encrypt - +RTS -N2 | ./dist/build/rsa1/rsa1 decrypt -
	echo testing | ./dist/build/rsa2/rsa2 encrypt - +RTS -N2 | ./dist/build/rsa2/rsa2 decrypt -
#	./dist/build/server2/server2
#	./dist/build/server/server
	./dist/build/sudoku1/sudoku1 sudoku17.1000.txt
	./dist/build/sudoku2/sudoku2 sudoku17.1000.txt +RTS -N2
	./dist/build/sudoku3/sudoku3 sudoku17.1000.txt +RTS -N2
	./dist/build/sudoku4/sudoku4 sudoku17.1000.txt +RTS -N2
	./dist/build/threadperf1/threadperf1
	./dist/build/threadperf2/threadperf2 +RTS -K32m
	./dist/build/timeout2/timeout2
	./dist/build/timeout/timeout

.PHONY: chanbench
chanbench:
	$(GHC) -DCHAN    -O2 chanbench.hs -o chanbench-chan   
	$(GHC) -DTCHAN   -O2 chanbench.hs -o chanbench-tchan  
	$(GHC) -DTQUEUE  -O2 chanbench.hs -o chanbench-tqueue 
	$(GHC) -DTBQUEUE -O2 chanbench.hs -o chanbench-tbqueue

chanbench-run: chanbench
	time ./chanbench-chan       0 10000000
	time ./chanbench-tchan      0 10000000
	time ./chanbench-tqueue     0 10000000
	time ./chanbench-tbqueue    0 10000000
	time ./chanbench-chan       1 10000000
# takes too long:
#	time ./chanbench-tchan      1 10000000
	time ./chanbench-tqueue     1 10000000
#	time ./chanbench-tbqueue    1 10000000
	time ./chanbench-chan       2 10000000
	time ./chanbench-tchan      2 10000000
	time ./chanbench-tqueue     2 10000000
	time ./chanbench-tbqueue    2 10000000

GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts

kmeans : kmeans.hs KMeansCommon.hs
	$(GHC) $(GHC_OPTS) kmeans

GenSamples : GenSamples.hs KMeansCommon.hs
	$(GHC) $(GHC_OPTS) GenSamples

points.bin : GenSamples
	./GenSamples 5 20000 40000

points : points.bin

view : points
	gnuplot -e 'set terminal png; plot "points"' >points.png; eog points.png    

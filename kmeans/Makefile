GHC = ghc
GHC_OPTS = -O2

GenSamples : GenSamples.hs KMeansCommon.hs
	$(GHC) $(GHC_OPTS) GenSamples

kmeans : kmeans.hs KMeansCommon.hs
	$(GHC) $(GHC_OPTS) kmeans

points.bin : GenSamples
	./GenSamples 5 20000 40000

points : points.bin

view : points
	gnuplot -e 'set terminal png; plot "points"' >points.png; eog points.png    

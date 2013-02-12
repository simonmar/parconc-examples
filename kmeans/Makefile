GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts

kmeans : kmeans.hs KMeansCore.hs
	$(GHC) $(GHC_OPTS) kmeans

GenSamples : GenSamples.hs KMeansCore.hs
	$(GHC) $(GHC_OPTS) GenSamples

points : GenSamples
	./GenSamples 5 40000 60000

EncodePoints: EncodePoints.hs KMeansCore.hs
	$(GHC) $(GHC_OPTS) EncodePoints

points.bin : EncodePoints points
	./EncodePoints points points.bin

view : points
	gnuplot -e 'set terminal png; plot "points"' >points.png; eog points.png    

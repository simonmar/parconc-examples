GHC = ghc
GHC_OPTS = -O2 -threaded -rtsopts

all : points.bin

kmeans : kmeans.hs KMeansCore.hs
	$(GHC) $(GHC_OPTS) kmeans

GenSamples : GenSamples.hs KMeansCore.hs
	$(GHC) $(GHC_OPTS) GenSamples

points : GenSamples
	./GenSamples 5 50000 100000 1010

points.bin : points

view : points
	gnuplot -e 'set terminal png; plot "points"' >points.png; eog points.png    

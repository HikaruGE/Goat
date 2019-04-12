KidParser: Goat.hs GoatAST.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat


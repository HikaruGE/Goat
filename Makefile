Goat: Goat.hs GoatAST.hs PrettyPrinter.hs
	ghc Goat.hs

clean:
	rm -f *.o *.hi
	rm -f Goat


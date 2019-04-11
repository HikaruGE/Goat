KidParser: GoatParser.hs GoatAST.hs
	ghc GoatParser.hs

clean:
	rm -f *.o *.hi
	rm -f GoatParser


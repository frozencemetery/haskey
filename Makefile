default: pwstore

pwstore:
	runhaskell Setup.hs build

clean:
	rm -f *.hi *.o
	rm -rf dist/*


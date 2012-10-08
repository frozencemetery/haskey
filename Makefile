default: pwstore

configure:
	runhaskell Setup.hs configure

pwstore: configure
	runhaskell Setup.hs build

clean:
	rm -f *.hi *.o
	rm -rf dist


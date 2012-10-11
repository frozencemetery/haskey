default: pwstore

pwstore:
	runhaskell Setup.hs build

install:
	cp dist/build/pwstore/pwstore /usr/local/bin/.
	cp scripts/* /usr/local/bin/.

clean:
	rm -f *.hi *.o
	rm -rf dist/*


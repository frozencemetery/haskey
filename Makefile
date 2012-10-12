default: pwman

pwman:
	runhaskell Setup.hs build

install:
	cp dist/build/pwman/pwman /usr/local/bin/.
	cp scripts/* /usr/local/bin/.

clean:
	rm -f *.hi *.o
	rm -rf dist/*


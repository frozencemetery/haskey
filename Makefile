default: haskey

haskey:
	runhaskell Setup.hs build

install:
	cp dist/build/haskey/haskey /usr/local/bin/.
	cp scripts/* /usr/local/bin/.

clean:
	rm -f *.hi *.o
	rm -rf dist/*


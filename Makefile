default: haskey

haskey:
	cabal build

install:
	cabal install
	cp scripts/* /usr/local/bin/.

clean:
	rm -f *.hi *.o
	rm -rf dist/*


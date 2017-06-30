#!/bin/bash -ex

export DEBIAN_FRONTEND=noninteractive

apt update

apt -y install libxtst-dev ghc cabal-install \
    libghc-{random,x11,gtk,deepseq-generics}-dev

cabal update
cabal install xtest

./configure
make
make install

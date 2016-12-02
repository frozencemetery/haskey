#!/bin/bash -ex

export DEBIAN_FRONTEND=noninteractive

# gotta go fast
sed -i 's/deb.debian.org/httpredir.debian.org/g' /etc/apt/sources.list
apt-get update

apt-get -y install libxtst-dev ghc cabal-install \
        libghc-{random,x11,gtk}-dev

cabal update
cabal install xtest

./configure
make
make install

#!/bin/bash

sudo apt-get install libgl1-mesa-dev libglc-dev freeglut3-dev libedit-dev libglw1-mesa libglw1-mesa-dev
sudo apt-get install ghc
wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-src.tar.bz2
tar xjf ghc-7.6.3-src.tar.bz2
cd ghc-7.6.3
./configure
make -j 8
sudo make install
wget http://lambda.haskell.org/platform/download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz
tar xzf haskell-platform-2013.2.0.0.tar.gz
cd haskell-platform-2013.2.0.0
./configure
make
sudo make install
cabal update
cabal install cabal-install
cabal install elm
cabal install elm-server
exit 0

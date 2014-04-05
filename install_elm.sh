#!/bin/bash

exec sudo apt-get install libgl1-mesa-dev libglc-dev freeglut3-dev libedit-dev libglw1-mesa libglw1-mesa-dev
exec sudo apt-get install ghc
exec wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-src.tar.bz2
exec tar xjf ghc-7.6.3-src.tar.bz2
exec cd ghc-7.6.3
exec ./configure
exec make -j 8
exec sudo make install
exec wget
exec http://lambda.haskell.org/platform/download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz
exec tar xzvf haskell-platform-2013.2.0.0.tar.gz
exec cd haskell-platform-2013.2.0.0
exec ./configure
exec make
exec sudo make install
exec cabal update
exec cabal install cabal-install
exec cabal install elm
exec cabal install elm-server
exit 0

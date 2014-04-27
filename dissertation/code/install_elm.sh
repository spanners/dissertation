#!/bin/bash

# This script installs:
#
# 1. GHC 7.6.3 (The Glasgow Haskell Compiler)
# 2. The Haskell Platform 2013.2.0.0
# 3. The Elm language
# 4. The Elm server

# Configuration
#
NUM_CPUS=8 # Eight cpus


# After this script has finished successfully,
#  you can install elm-lang.org

echo "This will take a while.. go make some tea.. and dinner"
sudo apt-get install libgl1-mesa-dev libglc-dev freeglut3-dev 
sudo apt-get install libedit-dev libglw1-mesa libglw1-mesa-dev
sudo apt-get install ghc
wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-src.tar.bz2
tar xjf ghc-7.6.3-src.tar.bz2
cd ghc-7.6.3/mk
cp build.mk.sample build.mk
sed -i 's/^#BuildFlavour = quick/BuildFlavour = quick/' build.mk
cd ..
./configure
make -j $NUM_PROCS
sudo make install
cd
platform="2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz"
wget "http://lambda.haskell.org/platform/download/"$platform
tar xzvf haskell-platform-2013.2.0.0.tar.gz
cd haskell-platform-2013.2.0.0
./configure
make
sudo make install
cabal update
cabal install cabal-install
cabal install elm
cabal install elm-server
exit 0

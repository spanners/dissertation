1. Install ghc 7.6.3

    1. `sudo apt-get install libgl1-mesa-dev libglc-dev freeglut3-dev
       libedit-dev libglw1-mesa libglw1-mesa-dev`
    2. `sudo apt-get install ghc`
    3. `wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-src.tar.bz2`
    4. `tar xjf ghc-7.6.3-src.tar.bz2`
    5. `cd ghc-7.6.3`
    6. `./configure`
    7. `make -j 8`
    8. `sudo make install`
    9. `wget`
    1. `http://lambda.haskell.org/platform/download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz`
    1. `tar xzvf haskell-platform-2013.2.0.0.tar.gz`
    1. `cd haskell-platform-2013.2.0.0`
    1. `./configure`
    1. `make`
    1. `sudo make install`

2. `cabal update`
3. `cabal install cabal-install`
4. `cabal install elm`
5. `cabal install elm-server`

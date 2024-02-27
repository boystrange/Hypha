REQUIRES
--------

  Install all of these at once with the Haskell platform:

  GHC
  Alex
  Happy

  Additionally, some cabal packages and GLPK are necessary. In OS X
  with homebrew they can be installed with the following sequence of
  commands:

  brew tap homebrew/science
  brew install glpk
  brew install lp_solve 
  cabal install base-unicode-symbols
  cabal install containers-unicode-symbols
  cabal install multiset
  cabal install wl-pprint
  cabal install glpk-hs

  Installing lp_solve is not required, unless one plans to use it for
  solving level/ticket constraints.

  If GLPK is installed using MacPorts in MacPorts' standard
  installation directory, then glpk-hs must be given flags to find the
  required binaries:

  cabal install glpk-hs --extra-include-dirs=/opt/local/include --extra-lib-dirs=/opt/local/lib

COMPILING
---------

  cd src
  make

TESTING
-------

  cd examples
  make

DOCUMENTATION
-------------

  open html/index.html

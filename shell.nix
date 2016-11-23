with import <nixpkgs> {};
runCommand "tkve" {
    buildInputs = [ stack
                    haskell.compiler.ghc801
                    haskellPackages.hscolour
                    haskellPackages.haddock
                    lmdb
                  ];
    LD_LIBRARY_PATH="${lmdb}/lib";
} ""

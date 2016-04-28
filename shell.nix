with (import <nixpkgs> {}); with builtins;

stdenv.mkDerivation {

  name = "myEnv";

  buildInputs = if builtins.getEnv "CI" == "true" 
    then [
      rocksdb
      haskell.packages.lts-5_9.ghc
    ]
    else [
      haskell.packages.lts-5_9.ghc
    ];
   

  STACK_IN_NIX_EXTRA_ARGS
      = " --extra-lib-dirs=${rocksdb}/lib" 
      + " --extra-include-dirs=${rocksdb}/include" 
  ;
}

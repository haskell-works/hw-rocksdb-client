with (import <nixpkgs> {});
with builtins;

let
  # native packages installed by nix in CCI, but not locally
  nativePackages = [ rocksdb ];
  
  # Packages that are downloaded from Hackage in CCI but referenced locally otherwise
  # in a format of { package1 = localPath2; package2 = localPath2; }
  localPackages  = {};
  
  # Other non-stackage dependencies, such as LTE itself or/and hackage packages
  otherPackages  = [ haskell.packages.lts-5_9.ghc ];
  
  ################### INTERNAL HANDWAVING #####################
  isCI = if builtins.getEnv "CI" != "" then true else false;
  
  localDeps = if isCI
      then builtins.attrNames localPackages
      else builtins.attrValues localPackages;
  
  packages = if isCI 
      then builtins.concatLists [ nativePackages localDeps ]
      else builtins.concatLists [ localDeps ];
      
  stackArgs = if isCI
      then builtins.foldl' (x: y: x + " --extra-lib-dirs=${y}/lib --extra-include-dirs=${y}/include") "" nativePackages
      else " --extra-lib-dirs=/usr/local/lib --extra-include-dirs=/usr/local/include";
  
  ######################## DERIVIATION ########################
in stdenv.mkDerivation {
  
  name = "rocksdbClientEnv";

  buildInputs = builtins.concatLists [ packages otherPackages ];

  STACK_IN_NIX_EXTRA_ARGS = stackArgs;
}

{ mkDerivation, base, bytestring, c2hs, data-default, directory
, errors, exceptions, filepath, hspec, mtl, QuickCheck, resourcet
, rocksdb, stdenv, tasty, tasty-quickcheck, temporary, transformers
}:
mkDerivation {
  pname = "rocksdb-client";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring data-default errors exceptions filepath resourcet
    transformers
  ];
  librarySystemDepends = [ rocksdb ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [
    base bytestring data-default directory errors exceptions filepath
    hspec mtl QuickCheck tasty tasty-quickcheck temporary transformers
  ];
  testSystemDepends = [ rocksdb ];
  homepage = "http://github.com/haskell-works/rocksdb-client";
  description = "Haskell client to RocksDB";
  license = stdenv.lib.licenses.mit;
}

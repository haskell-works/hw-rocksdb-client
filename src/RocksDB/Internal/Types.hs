module RocksDB.Internal.Types
where

newtype RocksDBError = RocksDBError String deriving (Show, Eq)

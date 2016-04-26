module RocksDB.Types

where

newtype RocksDBError = RocksDBError String deriving (Show, Eq)

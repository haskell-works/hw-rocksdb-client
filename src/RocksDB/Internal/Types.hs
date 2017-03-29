module RocksDB.Internal.Types
where

import Control.Exception

newtype RocksDBError = RocksDBError String deriving (Show, Eq)

instance Exception RocksDBError

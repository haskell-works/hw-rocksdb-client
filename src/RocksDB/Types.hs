module RocksDB.Types
( module RocksDB.Internal.Types
, module RocksDB.Types
)
where

import Control.Monad.Trans.Except
import RocksDB.Internal.C
import RocksDB.Internal.Types

newtype ErrorIfExists = ErrorIfExists Bool

data RocksDB = RocksDB OptionsFPtr RocksDBFPtr

type RocksDBResult a = ExceptT RocksDBError IO a

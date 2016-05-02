module RocksDB.Types
( module RocksDB.Internal.Types
, module RocksDB.Types
)
where

import           RocksDB.Internal.C
import           RocksDB.Internal.Types
import           Control.Monad.Trans.Except

newtype ErrorIfExists = ErrorIfExists Bool

data RocksDB = RocksDB OptionsFPtr RocksDBFPtr

type RocksDBResult a = ExceptT RocksDBError IO a

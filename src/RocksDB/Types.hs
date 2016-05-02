module RocksDB.Types

where

import           RocksDB.Internal.C
import           RocksDB.Internal.Types
import           Control.Monad.Trans.Except


data RocksDB = RocksDB OptionsFPtr RocksDBFPtr

type RocksDBResult a = ExceptT RocksDBError IO a

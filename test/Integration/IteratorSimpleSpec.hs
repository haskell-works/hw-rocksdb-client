{-# LANGUAGE OverloadedStrings #-}
module Integration.IteratorSimpleSpec ( spec )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           RocksDB
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           Matchers
import           Data.ByteString.Char8 (singleton)


dbPath :: IO FilePath
dbPath  = (</> "rocksdb_iter_simple_example") <$> getTemporaryDirectory

spec :: Spec
spec = describe "RocksDB.Integration.IteratorSimpleSpec" $
    it "iterate through" $ ensureSuccess $ do
        path <- liftIO dbPath
        db   <- open path (setCreateIfMissing True)

        wOpts <- defaultWriteOptions
        rOpts <- defaultReadOptions

        mapM_ (uncurry $ put db wOpts) [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]

        mvals <- multiGet' db rOpts ["a", "t", "z"]
        liftIO $ mvals `shouldBe` [("a", "a"), ("t", "t"), ("z", "z")]

        close db

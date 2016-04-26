{-# LANGUAGE OverloadedStrings #-}
module Integration.SimpleSpec

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

dbPath  = (</> "rocksdb_simple_example") <$> getTemporaryDirectory

spec :: Spec
spec = describe "Kafka.IntegrationSpec" $
    it "put, get, delete" $ ensureSuccess $ do
        path <- liftIO dbPath
        db   <- open path (setCreateIfMissing True)

        wOpts <- defaultWriteOptions
        rOpts <- defaultReadOptions

        put db wOpts "MyKey" "MyValue"
        val <- get db rOpts "MyKey"
        liftIO $ val `shouldBe` Just "MyValue"

        mvals' <- multiGet' db rOpts ["MyKey", "noKey"]
        liftIO $ mvals' `shouldBe` [("MyKey", "MyValue")]

        delete db wOpts "MyKey"
        dval <- get db rOpts "MyKey"
        liftIO $ dval `shouldBe` Nothing

        close db

ensureSuccess :: RocksDBResult () -> IO ()
ensureSuccess r = do
    res <- runExceptT r
    res `shouldBe` Right()

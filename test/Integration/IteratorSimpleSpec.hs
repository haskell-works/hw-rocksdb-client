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
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (singleton)
import Control.Concurrent

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_iter_simple_example") <$> getTemporaryDirectory

spec :: Spec
spec = describe "RocksDB.Integration.IteratorSimpleSpec" $
    it "iterate through" $ ensureSuccess $ do
        path <- liftIO dbPath
        db   <- open path (setCreateIfMissing True)

        wOpts <- defaultWriteOptions
        rOpts <- defaultReadOptions

        let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]

        mapM_ (uncurry $ put db wOpts) input

        res <- runIterator db rOpts [] (flip (:))
        liftIO $ reverse res `shouldBe` input

        res2 <- runIteratorFrom db rOpts [] "f" (flip (:))
        liftIO $ reverse res2 `shouldBe` drop 5 input

        res3 <- runIterator' db rOpts [] acc
        liftIO $ reverse res3 `shouldBe` take 1 input

        res4 <- runIteratorFrom' db rOpts [] "g" acc
        liftIO $ reverse res4 `shouldBe` (take 1 . drop 6) input

        close db

acc :: [(ByteString, ByteString)] -> (ByteString, ByteString) -> Iterate [(ByteString, ByteString)]
acc a b = IterateCompleted (b : a)

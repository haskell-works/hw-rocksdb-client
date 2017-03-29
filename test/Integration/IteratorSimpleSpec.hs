{-# LANGUAGE OverloadedStrings #-}
module Integration.IteratorSimpleSpec ( spec )
where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           RocksDB.Except
import           RocksDB.Iterator
import           RocksDB.Options
import           RocksDB.ReadOptions
import           RocksDB.WriteOptions
import           System.Directory
import           System.FilePath
import           Test.Hspec
import           Matchers
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (singleton)

dbPath :: IO FilePath
dbPath  = (</> "rocksdb_iter_simple_example") <$> getTemporaryDirectory

spec :: Spec
spec = beforeAll createContext . afterAll (\(db, _, _) -> ensureSuccess $ close db) $
  describe "RocksDB.Integration.IteratorSimpleSpec" $ do
    context "forward iterators" $ do
      it "iterate through" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIterator db rOpts [] (flip (:))
         liftIO $ reverse res `shouldBe` input

      it "iterate from statring point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorFrom db rOpts [] "f" (flip (:))
         liftIO $ reverse res `shouldBe` drop 5 input

      it "breakable iterator" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIterator' db rOpts [] takeOne
         liftIO $ reverse res `shouldBe` take 1 input

      it "breakable iterator from starting point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorFrom' db rOpts [] "g" takeOne
         liftIO $ reverse res `shouldBe` (take 1 . drop 6) input

    context "backwards iterators" $ do
      it "iterate throug" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwards db rOpts [] (flip (:))
         liftIO $ res `shouldBe` input

      it "iterate from statring point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwardsFrom db rOpts [] "f" (flip (:))
         liftIO $ res `shouldBe` take 6 input

      it "breakable iterator" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwards' db rOpts [] takeOne
         liftIO $ res `shouldBe` take 1 (reverse input)

      it "breakable iterator from starting point" $ \(db, rOpts, input) -> ensureSuccess $ do
         res <- runIteratorBackwardsFrom' db rOpts [] "g" takeOne
         liftIO $ res `shouldBe` (take 1 . drop 6) input

createContext :: IO (RocksDB, ReadOptions, [(ByteString, ByteString)])
createContext = do
  res <- runExceptT create
  case res of
    Right x -> return x
    Left _ -> undefined
  where
    create = do
      path  <- liftIO dbPath
      db    <- open path (setCreateIfMissing True)
      wOpts <- defaultWriteOptions
      rOpts <- defaultReadOptions
      let input =  [(bs, bs) | x <- ['a' .. 'z'], let bs = singleton x]
      mapM_ (uncurry $ put db wOpts) input
      return (db, rOpts, input)

takeOne :: [(ByteString, ByteString)] -> (ByteString, ByteString) -> Iterate [(ByteString, ByteString)]
takeOne a b = IterateCompleted (b : a)

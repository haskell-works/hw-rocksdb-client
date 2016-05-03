module Matchers where

import Control.Monad.Trans.Except
import RocksDB
import Test.Hspec

ensureSuccess :: RocksDBResult () -> IO ()
ensureSuccess r = do
    res <- runExceptT r
    res `shouldBe` Right()

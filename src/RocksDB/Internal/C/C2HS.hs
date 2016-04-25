module RocksDB.Internal.C.C2HS where

import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString  (ByteString, packCString, packCStringLen,
                                   useAsCString, useAsCStringLen)
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           RocksDB.Types

enumToCInt :: Enum a => a -> CInt
enumToCInt = fromIntegral . fromEnum
{-# INLINE enumToCInt #-}

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral
{-# INLINE cIntToEnum #-}

cIntConv :: (Integral a, Num b) =>  a -> b
cIntConv = fromIntegral
{-# INLINE cIntConv #-}

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}

numToBool :: (Num b, Eq b) => b -> Bool
numToBool 1 = True
numToBool _ = False
{-# INLINE numToBool #-}

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek
{-# INLINE peekIntConv #-}

nullable :: (Ptr a -> b) -> Ptr a -> Maybe b
nullable conv ptr = if ptr == nullPtr then Nothing else Just $ conv ptr
{-# INLINE nullable #-}

nullableM :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
nullableM peeker ptr = if ptr == nullPtr
    then return Nothing
    else liftM Just $ peeker ptr
{-# INLINE nullableM #-}

toStringMaybe :: CString -> IO (Maybe String)
toStringMaybe = nullableM peekCString
{-# INLINE toStringMaybe #-}

peekStringMaybe :: Ptr CString -> IO (Maybe String)
peekStringMaybe x = peek x >>= toStringMaybe
{-# INLINE peekStringMaybe #-}

toBSMaybe :: CString -> IO (Maybe ByteString)
toBSMaybe = nullableM packCString
{-# INLINE toBSMaybe #-}

toBSLenMaybe :: CStringLen -> IO (Maybe ByteString)
toBSLenMaybe (p, l) =  if p == nullPtr || l < 0
    then return Nothing
    else Just <$> packCStringLen (p, l)
{-# INLINE toBSLenMaybe #-}

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr = nullable id
{-# INLINE toMaybePtr #-}

toBSLen :: CStringLen -> IO ByteString
toBSLen = packCStringLen
{-# INLINE toBSLen #-}

toBSLenArray :: Integral c => [(Ptr b, c)] -> IO [ByteString]
toBSLenArray = mapM $ packCStringLen . bimap castPtr cIntConv
{-# INLINE toBSLenArray #-}

toBSLenMaybeArray :: Integral c => [(Ptr b, c)] -> IO [Maybe ByteString]
toBSLenMaybeArray = mapM $ toBSLenMaybe . bimap castPtr cIntConv
{-# INLINE toBSLenMaybeArray #-}

withSplitCArray :: (Integral c, Storable c)
                => [(Ptr b, c)]
                -> (Int -> Ptr (Ptr b) -> Ptr c -> IO a)
                -> IO a
withSplitCArray ps f =
    let (vs, ss) = unzip ps
     in withArray vs $ \vs' ->
          withArrayLen ss $ \num ss' -> f num vs' ss'
{-# INLINE withSplitCArray #-}

withPtrArrayLen :: (Integral c, Storable c)
                => [s]
                -> (s -> ((Ptr b, c) -> IO a) -> IO a)
                -> (Int -> Ptr (Ptr b) -> Ptr c -> IO a)
                -> IO a
withPtrArrayLen str f g =
  withMany f str $ \lst -> do
    let lst' = bimap castPtr fromIntegral <$> lst
    withSplitCArray lst' $ \num v s -> g num v s
{-# INLINE withPtrArrayLen #-}

strToCStringLen :: Integral c => String -> ((Ptr b, c) -> IO a) -> IO a
strToCStringLen s f = withCStringLen s $ f . bimap castPtr fromIntegral
{-# INLINE strToCStringLen #-}

bsToCStringLen :: Integral c => ByteString -> ((Ptr b, c) -> IO a) -> IO a
bsToCStringLen s f = useAsCStringLen s $ f . bimap castPtr fromIntegral
{-# INLINE bsToCStringLen #-}

withCSPtrCArrayLen :: (Integral c, Storable c)
                   => [String]
                   -> (Int -> Ptr (Ptr b) -> Ptr c -> IO a)
                   -> IO a
withCSPtrCArrayLen str = withPtrArrayLen str strToCStringLen
{-# INLINE withCSPtrCArrayLen #-}

withCSPtrCArray :: [String] -> (Int -> Ptr CString -> IO a) -> IO a
withCSPtrCArray as f = withMany withCString as $ flip withArrayLen f
{-# INLINE withCSPtrCArray #-}

withBSPtrCArrayLen :: (Integral c, Storable c)
                   => [ByteString]
                   -> (Int -> Ptr (Ptr b) -> Ptr c -> IO a)
                   -> IO a
withBSPtrCArrayLen str = withPtrArrayLen str bsToCStringLen
{-# INLINE withBSPtrCArrayLen #-}

withBSPtrCArray :: [ByteString] -> (Int -> Ptr CString -> IO a) -> IO a
withBSPtrCArray as f = withMany useAsCString as $ flip withArrayLen f
{-# INLINE withBSPtrCArray #-}


withFPtrArrayLen :: [ForeignPtr a] -> (Int -> Ptr (Ptr a) -> IO b) -> IO b
withFPtrArrayLen as f =  withMany withForeignPtr as $ flip withArrayLen f
{-# INLINE withFPtrArrayLen #-}

withFPtrArray :: [ForeignPtr a] -> (Ptr (Ptr a) -> IO b) -> IO b
withFPtrArray as f =  withMany withForeignPtr as $ flip withArray f
{-# INLINE withFPtrArray #-}

withForeignPtr2 :: ForeignPtr a -> ForeignPtr b -> (Ptr a -> Ptr b -> IO c) -> IO c
withForeignPtr2 p1 p2 f =
    withForeignPtr p1 $ \p1' -> withForeignPtr p2 (f p1')
{-# INLINE withForeignPtr2 #-}

withForeignPtr3 :: ForeignPtr a -> ForeignPtr b -> ForeignPtr c -> (Ptr a -> Ptr b -> Ptr c -> IO d) -> IO d
withForeignPtr3 p1 p2 p3 f =
    withForeignPtr p1 $ \p1' ->
        withForeignPtr2 p2 p3 $ \p2' p3' -> f p1' p2' p3'
{-# INLINE withForeignPtr3 #-}

withKeyValues :: (Integral c, Storable c)
              => [(ByteString, ByteString)]
              -> (Int -> Ptr CString -> Ptr c -> Ptr CString -> Ptr c -> IO a)
              -> IO a
withKeyValues kvs f =
    let (ks, vs) = unzip kvs
     in withBSPtrCArrayLen ks $ \num ksv kss ->
            withBSPtrCArrayLen vs $ \_ vsv vss ->
                f num ksv kss vsv vss
{-# INLINE withKeyValues #-}

peekErrorMaybe :: Ptr CString -> IO (Maybe RocksDBError)
peekErrorMaybe x = peekStringMaybe x >>= \m -> return $ RocksDBError <$> m
{-# INLINE peekErrorMaybe #-}

eitherFromError :: Ptr CString -> IO a -> IO (Either RocksDBError a)
eitherFromError era f = do
    era' <- peekErrorMaybe era
    case era' of
      Just msg -> return $ Left msg
      Nothing -> Right <$> f
{-# INLINE eitherFromError #-}


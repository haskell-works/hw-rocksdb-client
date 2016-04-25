{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module RocksDB.Internal.C.CRocksDB where

import Foreign
import Foreign.C.Types
import Foreign.C.String
{#import RocksDB.Internal.C.CTypes#}
import Data.ByteString (ByteString)

import RocksDB.Types
import RocksDB.Internal.C.C2HS

#include <rocksdb/c.h>

c_rocksdb_open :: OptionsFPtr -> String -> IO (Either RocksDBError RocksDBFPtr)
c_rocksdb_open o n =
    withForeignPtr o $ \o' ->
      withCString n $ \n' ->
        alloca $ \era -> do
          res  <- {#call rocksdb_open #} o' n' era
          eitherFromError era (newForeignPtr_ res)

c_rocksdb_open_for_read_only :: OptionsFPtr -> String -> ErrorIfLogExists -> IO (Either RocksDBError RocksDBFPtr)
c_rocksdb_open_for_read_only o n e =
    withForeignPtr o $ \o' ->
      withCString n $ \n' ->
        alloca $ \era -> do
          res <- {#call rocksdb_open_for_read_only #} o' n' (boolToNum e) era
          eitherFromError era (newForeignPtr_ res)

c_rocksdb_backup_engine_open :: OptionsFPtr -> String -> IO (Either RocksDBError BackupEngineFPtr)
c_rocksdb_backup_engine_open o n =
    withForeignPtr o $ \o' ->
      withCString n $ \n' ->
        alloca $ \era -> do
          res <- {#call rocksdb_backup_engine_open #} o' n' era
          eitherFromError era (newForeignPtr_ res)

{#fun rocksdb_backup_engine_create_new_backup as c_rocksdb_backup_engine_create_new_backup
    {`BackupEngineFPtr', `RocksDBFPtr', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

c_rocksdb_restore_options_create :: IO RestoreOptionsFPtr
c_rocksdb_restore_options_create =
    {#call rocksdb_restore_options_create #} >>= newForeignPtr c_rocksdb_restore_options_destroyF

{#fun rocksdb_restore_options_destroy as c_rocksdb_restore_options_destroy
    {`RestoreOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_restore_options_destroy"
    c_rocksdb_restore_options_destroyF :: FunPtr (RestoreOptionsPtr -> IO ())

{#fun rocksdb_restore_options_set_keep_log_files as c_rocksdb_restore_options_set_keep_log_files
    {`RestoreOptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_backup_engine_restore_db_from_latest_backup as c_rocksdb_backup_engine_restore_db_from_latest_backup 
    {`BackupEngineFPtr', `String', `String', `RestoreOptionsFPtr',
      alloca- `Maybe RocksDBError' peekErrorMaybe*}  -> `()' #}

c_rocksdb_backup_engine_get_backup_info :: BackupEngineFPtr -> IO BackupEngineInfoFPtr
c_rocksdb_backup_engine_get_backup_info be =
    withForeignPtr be $ \be' ->
      {#call rocksdb_backup_engine_get_backup_info #} be' >>= newForeignPtr c_rocksdb_backup_engine_info_destroyF

{#fun rocksdb_backup_engine_info_count as c_rocksdb_backup_engine_info_count
    {`BackupEngineInfoFPtr'} -> `Int' #}

{#fun rocksdb_backup_engine_info_timestamp as c_rocksdb_backup_engine_info_timestamp
    {`BackupEngineInfoFPtr', `Int'} -> `CInt64T' cIntConv #}

{#fun rocksdb_backup_engine_info_backup_id as c_rocksdb_backup_engine_info_backup_id
    {`BackupEngineInfoFPtr', `Int'} -> `CInt32T' cIntConv #}

{#fun rocksdb_backup_engine_info_size as c_rocksdb_backup_engine_info_size
    {`BackupEngineInfoFPtr', `Int'} -> `CUInt64T' cIntConv #}

{#fun rocksdb_backup_engine_info_number_files as c_rocksdb_backup_engine_info_number_files
    {`BackupEngineInfoFPtr', `Int'} -> `CUInt32T' cIntConv #}

{#fun rocksdb_backup_engine_info_destroy as c_rocksdb_backup_engine_info_destroy
    {`BackupEngineInfoFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_backup_engine_info_destroy"
    c_rocksdb_backup_engine_info_destroyF :: FunPtr (BackupEngineInfoPtr -> IO ())

{#fun rocksdb_backup_engine_close as c_rocksdb_backup_engine_close
    {`BackupEngineFPtr'} -> `()' #}

----------------------------------------------
-- Column Families
----------------------------------------------

{#fun rocksdb_column_family_handle_destroy as c_rocksdb_column_family_handle_destroy
    {`ColumnFamilyHandleFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_column_family_handle_destroy"
    c_rocksdb_column_family_handle_destroyF :: FunPtr (ColumnFamilyHandlePtr -> IO ())

c_rocksdb_open_column_families :: OptionsFPtr
                               -> String
                               -> [(String, OptionsFPtr)]
                               -> IO (Either RocksDBError (RocksDBFPtr, [ColumnFamilyHandleFPtr]))
c_rocksdb_open_column_families dbo nm cfs =
    let (names, copts) = unzip cfs
        toCFHandle = newForeignPtr c_rocksdb_column_family_handle_destroyF
     in withForeignPtr dbo $ \dbo' ->
          withCString nm $ \nm' ->
            withCSPtrCArray names $ \num names' ->
              withFPtrArray copts $ \copts'->
                alloca $ \era ->
                alloca $ \chs -> do
                  db <- {#call rocksdb_open_column_families#}
                            dbo' nm' (fromIntegral num) names' copts' chs era
                  eitherFromError era $ do
                    chs'  <- peekArray num chs
                    chs'' <- mapM toCFHandle chs'
                    db'   <- newForeignPtr_ db
                    return (db', chs'')

c_rocksdb_open_for_read_only_column_families :: OptionsFPtr
                               -> String
                               -> [(String, OptionsFPtr)]
                               -> ErrorIfLogExists
                               -> IO (Either RocksDBError (RocksDBFPtr, [ColumnFamilyHandleFPtr]))
c_rocksdb_open_for_read_only_column_families dbo nm cfs e =
    let (names, copts) = unzip cfs
        toCFHandle = newForeignPtr c_rocksdb_column_family_handle_destroyF
     in withForeignPtr dbo $ \dbo' ->
          withCString nm $ \nm' ->
            withCSPtrCArray names $ \num names' ->
              withFPtrArray copts $ \copts'->
                alloca $ \era ->
                alloca $ \chs -> do
                  db <- {#call rocksdb_open_for_read_only_column_families#}
                             dbo' nm' (fromIntegral num) names' copts' chs (boolToNum e) era
                  eitherFromError era $ do
                    chs'  <- peekArray num chs
                    chs'' <- mapM toCFHandle chs'
                    db'   <- newForeignPtr_ db
                    return (db', chs'')

c_rocksdb_list_column_families :: OptionsFPtr
                               -> String
                               -> IO (Either RocksDBError [String])
c_rocksdb_list_column_families dbo nm =
    withForeignPtr dbo $ \dbo' ->
      withCString nm $ \nm' ->
        alloca $ \sz ->
        alloca $ \era -> do
          nma <- {#call rocksdb_list_column_families #} dbo' nm' sz era
          eitherFromError era $ do
            sz'    <- peek sz
            names  <- peekArray (cIntConv sz') nma
            names' <- mapM peekCString names
            return names'

c_rocksdb_create_column_family :: RocksDBFPtr
                               -> OptionsFPtr
                               -> String
                               -> IO (Either RocksDBError ColumnFamilyHandleFPtr)
c_rocksdb_create_column_family db copt nm =
    withForeignPtr2 db copt $ \db' copt' ->
      withCString nm $ \nm' ->
        alloca $ \era -> do
          cfn  <- {#call rocksdb_create_column_family #} db' copt' nm' era
          eitherFromError era $ do
            cfn' <- newForeignPtr c_rocksdb_column_family_handle_destroyF cfn
            return cfn'

{#fun rocksdb_drop_column_family as c_rocksdb_drop_column_family
    {`RocksDBFPtr', `ColumnFamilyHandleFPtr', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

----------------------------------------------
-- RocksDB General
----------------------------------------------

{#fun rocksdb_close as c_rocksdb_close
    {`RocksDBFPtr'} -> `()' #}

{#fun rocksdb_put as c_rocksdb_put
    {`RocksDBFPtr', `WriteOptionsFPtr',
      bsToCStringLen* `ByteString'&,
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_put_cf as c_rocksdb_put_cf
    {`RocksDBFPtr', `WriteOptionsFPtr', `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_delete as c_rocksdb_delete
    {`RocksDBFPtr', `WriteOptionsFPtr',
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_delete_cf as c_rocksdb_delete_cf
    {`RocksDBFPtr', `WriteOptionsFPtr',
     `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_merge as c_rocksdb_merge
    {`RocksDBFPtr', `WriteOptionsFPtr',
      bsToCStringLen* `ByteString'&,
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}


{#fun rocksdb_merge_cf as c_rocksdb_merge_cf
    {`RocksDBFPtr', `WriteOptionsFPtr',
     `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,
      bsToCStringLen* `ByteString'&,
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_write as c_rocksdb_write
    {`RocksDBFPtr', `WriteOptionsFPtr',
     `WriteBatchFPtr',
      alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

c_rocksdb_get :: RocksDBFPtr -> ReadOptionsFPtr -> ByteString -> IO (Either RocksDBError ByteString)
c_rocksdb_get db ro k = 
    withForeignPtr2 db ro $ \db' ro' ->
      bsToCStringLen k $ \(s, l) ->
        alloca $ \sz ->
        alloca $ \era -> do
            res <- {#call rocksdb_get#} db' ro' s l sz era
            eitherFromError era $ do
              sz' <- peek sz
              toBSLen (res, fromIntegral sz')

{#fun rocksdb_get_cf as c_rocksdb_get_cf
    {`RocksDBFPtr', `ReadOptionsFPtr',
     `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,
      alloca- `CULong' peek*,
      alloca- `Maybe String' peekStringMaybe*} -> `CString' #}

c_rocksdb_multi_get :: RocksDBFPtr
                    -> ReadOptionsFPtr
                    -> [ByteString]
                    -> IO (Either RocksDBError [Maybe ByteString])
c_rocksdb_multi_get db ro kp =
    withBSPtrCArrayLen kp $ \num kva ksa ->
        withForeignPtr db $ \dbp ->
          withForeignPtr ro $ \rop ->
            alloca $ \vva ->
            alloca $ \vsa ->
            alloca $ \era -> do
                {#call rocksdb_multi_get #} dbp rop (fromIntegral num) kva ksa vva vsa era
                eitherFromError era $ do
                  vva' <- peekArray num vva
                  vsa' <- peekArray num vsa
                  toBSLenMaybeArray (zip vva' vsa')

c_rocksdb_multi_get_cf :: RocksDBFPtr
                       -> ReadOptionsFPtr
                       -> [(ColumnFamilyHandleFPtr, ByteString)]
                       -> IO (Either RocksDBError [Maybe ByteString])
c_rocksdb_multi_get_cf db ro ckp =
    let (chs, kp) = unzip ckp
    in withBSPtrCArrayLen kp $ \num kva ksa ->
        withFPtrArrayLen chs $ \_ chl ->
          withForeignPtr2 db ro $ \dbp rop ->
            alloca $ \vva ->
            alloca $ \vsa ->
            alloca $ \era -> do
                {#call rocksdb_multi_get_cf #} dbp rop chl (fromIntegral num) kva ksa vva vsa era
                eitherFromError era $ do
                  vva' <- peekArray num vva
                  vsa' <- peekArray num vsa
                  toBSLenMaybeArray (zip vva' vsa')

----------------------------------------------
-- Iterator
----------------------------------------------

c_rocksdb_create_iterator :: RocksDBFPtr -> ReadOptionsFPtr -> IO IteratorFPtr
c_rocksdb_create_iterator db op =
    withForeignPtr2 db op $ \db' op' ->
      {#call rocksdb_create_iterator #} db' op' >>= newForeignPtr c_rocksdb_iter_destroyF

c_rocksdb_create_iterator_cf :: RocksDBFPtr -> ReadOptionsFPtr -> ColumnFamilyHandleFPtr -> IO IteratorFPtr
c_rocksdb_create_iterator_cf db op cf =
    withForeignPtr3 db op cf $ \db' op' cf' ->
      {#call rocksdb_create_iterator_cf #} db' op' cf' >>= newForeignPtr c_rocksdb_iter_destroyF

----------------------------------------------
-- Property values
----------------------------------------------

{#fun rocksdb_property_value as c_rocksdb_property_value
    {`RocksDBFPtr', `String'} -> `Maybe ByteString' toBSMaybe* #}

{#fun rocksdb_property_value_cf as c_rocksdb_property_value_cf
    {`RocksDBFPtr', `ColumnFamilyHandleFPtr', `String'} -> `Maybe ByteString' toBSMaybe* #}

----------------------------------------------
-- Snapshots
----------------------------------------------

{#fun rocksdb_create_snapshot as c_rocksdb_create_snapshot
    {`RocksDBFPtr'} -> `SnapshotFPtr' #}

{#fun rocksdb_release_snapshot as c_rocksdb_release_snapshot
    {`RocksDBFPtr', `SnapshotFPtr'} -> `()' #}

----------------------------------------------
-- Approximation
----------------------------------------------

c_rocksdb_approximate_sizes :: RocksDBFPtr
                            -> [(ByteString, ByteString)] -- ^ (start, limit) keys
                            -> IO [CUInt64T]
c_rocksdb_approximate_sizes db rs =
    withForeignPtr db $ \db' ->
        withKeyValues rs $ \num skv sks lkv lks ->
          alloca $ \sz -> do
            {#call rocksdb_approximate_sizes #} db' (fromIntegral num) skv sks lkv lks sz
            sz' <- peekArray num sz
            return sz'

c_rocksdb_approximate_sizes_cf :: RocksDBFPtr
                               -> ColumnFamilyHandleFPtr
                               -> [(ByteString, ByteString)]  -- ^ (start, limit) keys
                               -> IO [CUInt64T]
c_rocksdb_approximate_sizes_cf db cf rs =
    withForeignPtr2 db cf $ \db' cf' ->
        withKeyValues rs $ \num skv sks lkv lks ->
          alloca $ \sz -> do
            {#call rocksdb_approximate_sizes_cf #} db' cf' (fromIntegral num) skv sks lkv lks sz
            sz' <- peekArray num sz
            return sz'

----------------------------------------------

{#fun rocksdb_compact_range as c_rocksdb_compact_range
    {`RocksDBFPtr', bsToCStringLen* `ByteString'&, bsToCStringLen* `ByteString'&} -> `()' #}

{#fun rocksdb_compact_range_cf as c_rocksdb_compact_range_cf
    {`RocksDBFPtr', `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,  bsToCStringLen* `ByteString'&} -> `()' #}

{#fun rocksdb_delete_file as c_rocksdb_delete_file
    {`RocksDBFPtr', `String'} -> `()' #}

{#fun rocksdb_livefiles as c_rocksdb_livefiles
    {`RocksDBFPtr'} -> `LiveFilesFPtr' #}

{#fun rocksdb_flush as c_rocksdb_flush
    {`RocksDBFPtr', `FlushOptionsFPtr', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_disable_file_deletions as c_rocksdb_disable_file_deletions
    {`RocksDBFPtr', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_enable_file_deletions as c_rocksdb_enable_file_deletions
    {`RocksDBFPtr', `Bool', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

----------------------------------------------
-- Manage Operations
----------------------------------------------

{#fun rocksdb_destroy_db as c_rocksdb_destroy_db
    {`OptionsFPtr', `String', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

{#fun rocksdb_repair_db as c_rocksdb_repair_db
    {`OptionsFPtr', `String', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

----------------------------------------------
-- Iterator (again)
----------------------------------------------

{#fun rocksdb_iter_destroy as c_rocksdb_iter_destroy
    {`IteratorFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_iter_destroy"
    c_rocksdb_iter_destroyF :: FunPtr (IteratorPtr -> IO ())

{#fun rocksdb_iter_valid as c_rocksdb_iter_valid
    {`IteratorFPtr'} -> `Bool' #}

{#fun rocksdb_iter_seek_to_first as c_rocksdb_iter_seek_to_first
    {`IteratorFPtr'} -> `()' #}

{#fun rocksdb_iter_seek_to_last as c_rocksdb_iter_seek_to_last
    {`IteratorFPtr'} -> `()' #}

{#fun rocksdb_iter_seek as c_rocksdb_iter_seek
    {`IteratorFPtr', bsToCStringLen* `ByteString'&} -> `()' #}

{#fun rocksdb_iter_next as c_rocksdb_iter_next
    {`IteratorFPtr'} -> `()' #}

{#fun rocksdb_iter_prev as c_rocksdb_iter_prev
    {`IteratorFPtr'} -> `()' #}

c_rocksdb_iter_key :: IteratorFPtr -> IO (Maybe ByteString)
c_rocksdb_iter_key iter =
    withForeignPtr iter $ \iter' ->
        alloca $ \sz -> do
            res <- {#call rocksdb_iter_key #} iter' sz
            sz' <- peek sz
            toBSLenMaybe (res, fromIntegral sz')

c_rocksdb_iter_value :: IteratorFPtr -> IO (Maybe ByteString)
c_rocksdb_iter_value iter =
    withForeignPtr iter $ \iter' ->
        alloca $ \sz -> do
            res <- {#call rocksdb_iter_value #} iter' sz
            sz' <- peek sz
            toBSLenMaybe (res, fromIntegral sz')

{#fun rocksdb_iter_get_error as c_rocksdb_iter_get_error
    {`IteratorFPtr', alloca- `Maybe RocksDBError' peekErrorMaybe*} -> `()' #}

----------------------------------------------
-- Write batch
----------------------------------------------

c_rocksdb_writebatch_create :: IO WriteBatchFPtr
c_rocksdb_writebatch_create =
  {#call rocksdb_writebatch_create #} >>= newForeignPtr c_rocksdb_writebatch_destroyF

{#fun rocksdb_writebatch_create_from as c_rocksdb_writebatch_create_from
    {bsToCStringLen* `ByteString'&} -> `WriteBatchFPtr' #}

{#fun rocksdb_writebatch_destroy as c_rocksdb_writebatch_destroy {`WriteBatchFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_writebatch_destroy"
    c_rocksdb_writebatch_destroyF :: FunPtr (WriteBatchPtr -> IO ())

{#fun rocksdb_writebatch_clear as c_rocksdb_writebatch_clear {`WriteBatchFPtr'} -> `()' #}

{#fun rocksdb_writebatch_count as c_rocksdb_writebatch_count {`WriteBatchFPtr'} -> `Int' #}

{#fun rocksdb_writebatch_put as c_rocksdb_writebatch_put
    {`WriteBatchFPtr', bsToCStringLen* `ByteString'&, bsToCStringLen* `ByteString'&} -> `()' #}

{#fun rocksdb_writebatch_put_cf as c_rocksdb_writebatch_put_cf
    {`WriteBatchFPtr', `ColumnFamilyHandleFPtr',
      bsToCStringLen* `ByteString'&,
      bsToCStringLen* `ByteString'&} -> `()' #}

c_rocksdb_writebatch_putv :: WriteBatchFPtr
                          -> [(ByteString, ByteString)]
                          -> IO ()
c_rocksdb_writebatch_putv b kvs =
    withForeignPtr b $ \b' ->
        withKeyValues kvs $ \num ksv kss vsv vss ->
            {#call rocksdb_writebatch_putv #} b' (fromIntegral num) ksv kss (fromIntegral num) vsv vss

c_rocksdb_writebatch_putv_cf :: WriteBatchFPtr
                             -> ColumnFamilyHandleFPtr
                             -> [(ByteString, ByteString)]
                             -> IO ()
c_rocksdb_writebatch_putv_cf b ch kvs =
    withForeignPtr2 b ch $ \b' ch' ->
        withKeyValues kvs $ \num ksv kss vsv vss ->
           {#call rocksdb_writebatch_putv_cf #} b' ch' (fromIntegral num) ksv kss (fromIntegral num) vsv vss

{# fun rocksdb_writebatch_merge as c_rocksdb_writebatch_merge
    {`WriteBatchFPtr', bsToCStringLen* `ByteString'&, bsToCStringLen* `ByteString'&} -> `()' #}

{# fun rocksdb_writebatch_merge_cf as c_rocksdb_writebatch_merge_cf
    {`WriteBatchFPtr', `ColumnFamilyHandleFPtr', bsToCStringLen* `ByteString'&, bsToCStringLen* `ByteString'&} -> `()' #}

c_rocksdb_writebatch_mergev :: WriteBatchFPtr
                            -> [(ByteString, ByteString)]
                            -> IO ()
c_rocksdb_writebatch_mergev b kvs =
    withForeignPtr b $ \b' ->
        withKeyValues kvs $ \num ksv kss vsv vss ->
            {#call rocksdb_writebatch_mergev #} b' (fromIntegral num) ksv kss (fromIntegral num) vsv vss

c_rocksdb_writebatch_mergev_cf :: WriteBatchFPtr
                            -> ColumnFamilyHandleFPtr
                            -> [(ByteString, ByteString)]
                            -> IO ()
c_rocksdb_writebatch_mergev_cf b ch kvs =
    withForeignPtr2 b ch $ \b' ch' ->
        withKeyValues kvs $ \num ksv kss vsv vss ->
            {#call rocksdb_writebatch_mergev_cf #} b' ch' (fromIntegral num) ksv kss (fromIntegral num) vsv vss

{#fun rocksdb_writebatch_delete as c_rocksdb_writebatch_delete
    {`WriteBatchFPtr', bsToCStringLen* `ByteString'&} -> `()'  #}

{#fun rocksdb_writebatch_delete_cf as c_rocksdb_writebatch_delete_cf
    {`WriteBatchFPtr', `ColumnFamilyHandleFPtr', bsToCStringLen* `ByteString'&} -> `()'  #}

с_rocksdb_writebatch_deletev :: WriteBatchFPtr
                             -> [ByteString]
                             -> IO ()
с_rocksdb_writebatch_deletev b bs =
    withForeignPtr b $ \b' ->
        withBSPtrCArrayLen bs $ \num vs ss ->
            {# call rocksdb_writebatch_deletev #} b' (fromIntegral num) vs ss

с_rocksdb_writebatch_deletev_cf :: WriteBatchFPtr
                                -> ColumnFamilyHandleFPtr
                                -> [ByteString]
                                -> IO ()
с_rocksdb_writebatch_deletev_cf b ch bs =
    withForeignPtr2 b ch $ \b' ch' ->
        withBSPtrCArrayLen bs $ \num vs ss ->
            {# call rocksdb_writebatch_deletev_cf #} b' ch' (fromIntegral num) vs ss

{#fun rocksdb_writebatch_put_log_data as c_rocksdb_writebatch_put_log_data
    {`WriteBatchFPtr', bsToCStringLen* `ByteString'&} -> `()' #}

type WriteBatchPutCb = OpaquePtr -> CString -> CSize -> CString -> CSize -> IO ()
type WriteBatchDeletedCb = OpaquePtr -> CString -> CSize -> IO ()

foreign import ccall "wrapper" mkWriteBatchPutCb :: WriteBatchPutCb -> IO (FunPtr WriteBatchPutCb)

foreign import ccall "wrapper" mkWriteBatchDeletedCb :: WriteBatchDeletedCb -> IO (FunPtr WriteBatchDeletedCb)

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_iterate"
  c_rocksdb_writebatch_iterate :: WriteBatchPtr
                               -> OpaquePtr
                               -> FunPtr WriteBatchPutCb
                               -> FunPtr WriteBatchDeletedCb
                               -> IO ()

c_rocksdb_writebatch_data :: WriteBatchFPtr -> IO ByteString
c_rocksdb_writebatch_data b =
    withForeignPtr b $ \b' ->
        alloca $ \sz -> do
           res <- {# call rocksdb_writebatch_data #} b' sz
           sz' <- peek sz
           toBSLen (res, fromIntegral sz')

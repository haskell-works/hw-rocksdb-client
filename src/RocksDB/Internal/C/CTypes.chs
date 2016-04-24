{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module RocksDB.Internal.C.CTypes where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString

import RocksDB.Internal.C.C2HS

#include <rocksdb/c.h>

type OpaquePtr   = Ptr ()

type ErrorIfLogExists = Bool

type Destructor = OpaquePtr -> ()
type NameFun = OpaquePtr -> CString

-- | Make a destructor FunPtr
foreign import ccall "wrapper" mkDest :: Destructor -> IO (FunPtr Destructor)

-- | Make a name FunPtr
foreign import ccall "wrapper" mkName :: NameFun -> IO (FunPtr NameFun)

type CInt64T  = {#type int64_t  #}
type CInt32T  = {#type int32_t  #}
type CUInt32T = {#type uint32_t #}
type CUInt64T = {#type uint64_t #}

data CRocksDB
data CBackupEngine
data CBackupEngineInfo
data CRestoreOptions
data CCache
data CCompactionFilter
data CCompactionFilterContext
data CCompactionFilterFactory
data CComparator
data CEnv
data CFifoCompactionOptions
data CFilelock
data CFilterPolicy
data CFlushOptions
data CIterator
data CLogger
data CMergeOperator
data COptions
data CBlockBasedTableOptions
data CCuckooTableOptions
data CRandomFile
data CReadOptions
data CSeqFile
data CSliceTransform
data CSnapshot
data CWritableFile
data CWriteBatch
data CWriteOptions
data CUniversalCompactionOptions
data CLiveFiles
data CColumnFamilyHandle

type RocksDBPtr                    = Ptr CRocksDB
type BackupEnginePtr               = Ptr CBackupEngine
type BackupEngineInfoPtr           = Ptr CBackupEngineInfo
type RestoreOptionsPtr             = Ptr CRestoreOptions
type CachePtr                      = Ptr CCache
type CompactionFilterPtr           = Ptr CCompactionFilter
type CompactionFilterContextPtr    = Ptr CCompactionFilterContext
type CompactionFilterFactoryPtr    = Ptr CCompactionFilterFactory
type ComparatorPtr                 = Ptr CComparator
type EnvPtr                        = Ptr CEnv
type FifoCompactionOptionsPtr      = Ptr CFifoCompactionOptions
type FilelockPtr                   = Ptr CFilelock
type FilterPolicyPtr               = Ptr CFilterPolicy
type FlushOptionsPtr               = Ptr CFlushOptions
type IteratorPtr                   = Ptr CIterator
type LoggerPtr                     = Ptr CLogger
type MergeOperatorPtr              = Ptr CMergeOperator
type OptionsPtr                    = Ptr COptions
type BlockBasedTableOptionsPtr     = Ptr CBlockBasedTableOptions
type CuckooTableOptionsPtr         = Ptr CCuckooTableOptions
type RandomFilePtr                 = Ptr CRandomFile
type ReadOptionsPtr                = Ptr CReadOptions
type SeqFilePtr                    = Ptr CSeqFile
type SliceTransformPtr             = Ptr CSliceTransform
type SnapshotPtr                   = Ptr CSnapshot
type WritableFilePtr               = Ptr CWritableFile
type WriteBatchPtr                 = Ptr CWriteBatch
type WriteOptionsPtr               = Ptr CWriteOptions
type UniversalCompactionOptionsPtr = Ptr CUniversalCompactionOptions
type LiveFilesPtr                  = Ptr CLiveFiles
type ColumnFamilyHandlePtr         = Ptr CColumnFamilyHandle

{#pointer *rocksdb_t as RocksDBFPtr foreign -> CRocksDB #}
{#pointer *rocksdb_backup_engine_t as BackupEngineFPtr foreign -> CBackupEngine #}
{#pointer *rocksdb_backup_engine_info_t as BackupEngineInfoFPtr foreign -> CBackupEngineInfo #}
{#pointer *rocksdb_restore_options_t as RestoreOptionsFPtr foreign -> CRestoreOptions #}
{#pointer *rocksdb_cache_t as CacheFPtr foreign -> CCache #}
{#pointer *rocksdb_compactionfilter_t as CompactionFilterFPtr foreign -> CCompactionFilter #}
{#pointer *rocksdb_compactionfiltercontext_t as CompactionFilterContextFPtr foreign -> CCompactionFilterContext #}
{#pointer *rocksdb_compactionfilterfactory_t as CompactionFilterFactoryFPtr foreign -> CCompactionFilterFactory #}
{#pointer *rocksdb_env_t as EnvFPtr foreign -> CEnv #}
{#pointer *rocksdb_comparator_t as ComparatorFPtr foreign -> CComparator #}
{#pointer *rocksdb_fifo_compaction_options_t as FifoCompactionOptionsFPtr foreign -> CFifoCompactionOptions #}
{#pointer *rocksdb_filelock_t as FilelockFPtr foreign -> CFilelock #}
{#pointer *rocksdb_filterpolicy_t as FilterPolicyFPtr foreign -> CFilterPolicy #}
{#pointer *rocksdb_flushoptions_t as FlushOptionsFPtr foreign -> CFlushOptions#}
{#pointer *rocksdb_iterator_t as IteratorFPtr foreign -> CIterator #}
{#pointer *rocksdb_logger_t as LoggerFPtr foreign -> CLogger #}
{#pointer *rocksdb_mergeoperator_t as MergeOperatorFPtr foreign -> CMergeOperator #}
{#pointer *rocksdb_options_t as OptionsFPtr foreign -> COptions #}
{#pointer *rocksdb_block_based_table_options_t as BlockBasedTableOptionsFPtr foreign -> CBlockBasedTableOptions #}
{#pointer *rocksdb_cuckoo_table_options_t as CuckooTableOptionsFPtr foreign -> CCuckooTableOptions #}
{#pointer *rocksdb_randomfile_t as RandomFileFPtr foreign -> CRandomFile #}
{#pointer *rocksdb_readoptions_t as ReadOptionsFPtr foreign -> CReadOptions #}
{#pointer *rocksdb_seqfile_t as SeqFileFPtr foreign -> CSeqFile #}
{#pointer *rocksdb_slicetransform_t as SliceTransformFPtr foreign -> CSliceTransform #}
{#pointer *rocksdb_snapshot_t as SnapshotFPtr foreign -> CSnapshot #}
{#pointer *rocksdb_writablefile_t as WritableFileFPtr foreign -> CWritableFile #}
{#pointer *rocksdb_writebatch_t as WriteBatchFPtr foreign -> CWriteBatch #}
{#pointer *rocksdb_writeoptions_t as WriteOptionsFPtr foreign -> CWriteOptions #}
{#pointer *rocksdb_universal_compaction_options_t as UniversalCompactionOptionsFPtr foreign -> CUniversalCompactionOptions #}
{#pointer *rocksdb_livefiles_t as LiveFilesFPtr foreign -> CLiveFiles #}
{#pointer *rocksdb_column_family_handle_t as ColumnFamilyHandleFPtr foreign -> CColumnFamilyHandle #}


----------------------------------------------
-- Compaction Filter
----------------------------------------------

type CCompactionFilterCb = OpaquePtr   -- ^ state
                        -> CInt        -- ^ level
                        -> CString     -- ^ key
                        -> CSize       -- ^ key_length
                        -> CString     -- ^ existing_value
                        -> CString     -- ^ value_length
                        -> Ptr CString -- ^ new_value
                        -> Ptr CSize   -- ^ new_value_length
                        -> Ptr CUChar  -- ^ value_changed
                        -> IO CUChar

foreign import ccall "wrapper" mkCompactFilterCb ::
    CCompactionFilterCb -> IO (FunPtr CCompactionFilterCb)

foreign import ccall safe "rocksdb/c.h rocksdb_compactionfilter_create"
    c_rocksdb_compactionfilter_create :: OpaquePtr
                                      -> FunPtr Destructor
                                      -> FunPtr CCompactionFilterCb
                                      -> FunPtr NameFun
                                      -> IO CompactionFilterPtr

{#fun rocksdb_compactionfilter_destroy as c_rocksdb_compactionfilter_destroy
    {`CompactionFilterFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_compactionfilter_destroy"
    c_rocksdb_compactionfilter_destroyF :: FunPtr (CompactionFilterFPtr -> IO ())

----------------------------------------------
-- Compaction Filter Context
----------------------------------------------

{#fun rocksdb_compactionfiltercontext_is_full_compaction as c_rocksdb_compactionfiltercontext_is_full_compaction
    {`CompactionFilterContextFPtr'} -> `Bool' numToBool #}

{#fun rocksdb_compactionfiltercontext_is_manual_compaction as c_rocksdb_compactionfiltercontext_is_manual_compaction
    {`CompactionFilterContextFPtr'} -> `Bool' numToBool #}

----------------------------------------------
-- Compaction Filter Factory
----------------------------------------------

type CCompactionFilterFactoryCb = OpaquePtr
                               -> CompactionFilterContextPtr
                               -> IO CompactionFilterPtr

foreign import ccall "wrapper" mkCompactFilterFactoryCb ::
    CCompactionFilterFactoryCb -> IO (FunPtr CCompactionFilterFactoryCb)

foreign import ccall safe "rocksdb/c.h rocksdb_compactionfilterfactory_create"
    c_rocksdb_compactionfilterfactory_create :: OpaquePtr
                                             -> FunPtr Destructor
                                             -> FunPtr CCompactionFilterFactoryCb
                                             -> FunPtr NameFun
                                             -> IO CompactionFilterFactoryPtr

{#fun rocksdb_compactionfilterfactory_destroy as c_rocksdb_compactionfilterfactory_destroy
    {`CompactionFilterFactoryFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_compactionfilterfactory_destroy"
    c_rocksdb_compactionfilterfactory_destroyF :: FunPtr (CompactionFilterFactoryFPtr -> IO ())

----------------------------------------------
-- Comparator
----------------------------------------------

type CompareFun = OpaquePtr -> CString -> CSize -> CString -> CSize -> IO CInt

foreign import ccall safe "rocksdb/c.h rocksdb_comparator_create"
  c_rocksdb_comparator_create :: OpaquePtr
                              -> FunPtr Destructor
                              -> FunPtr CompareFun
                              -> FunPtr NameFun
                              -> IO ComparatorPtr

{#fun rocksdb_comparator_destroy as c_rocksdb_comparator_destroy
    {`ComparatorFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_comparator_destroy"
    c_rocksdb_comparator_destroyF :: FunPtr (ComparatorPtr -> IO ())


----------------------------------------------
-- Filter policy
----------------------------------------------

type CCreateFilterCb = OpaquePtr
                    -> Ptr CString -- ^ key array
                    -> Ptr CSize   -- ^ key length array
                    -> CInt        -- ^ num keys
                    -> Ptr CSize   -- ^ filter length
                    -> IO CString  -- ^ the filter
type CKeyMayMatchCb = OpaquePtr
                   -> CString     -- ^ key
                   -> CSize       -- ^ key length
                   -> CString     -- ^ filter
                   -> CSize       -- ^ filter length
                   -> IO CUChar   -- ^ whether key is in filter

-- | Make a FunPtr to a user-defined create_filter function
foreign import ccall "wrapper" mkCreateFilterCb :: CCreateFilterCb -> IO (FunPtr CCreateFilterCb)

-- | Make a FunPtr to a user-defined key_may_match function
foreign import ccall "wrapper" mkKeyMayMatchCb :: CKeyMayMatchCb -> IO (FunPtr CKeyMayMatchCb)

foreign import ccall safe "rocksdb/c.h rocksdb_filterpolicy_create"
  c_rocksdb_filterpolicy_create :: OpaquePtr
                                -> FunPtr Destructor
                                -> FunPtr CCreateFilterCb
                                -> FunPtr CKeyMayMatchCb
                                -> FunPtr NameFun
                                -> IO FilterPolicyPtr

c_rocksdb_filterpolicy_create_bloom :: Int -> IO FilterPolicyFPtr
c_rocksdb_filterpolicy_create_bloom n =
    {#call rocksdb_filterpolicy_create_bloom #} (cIntConv n) >>= newForeignPtr c_rocksdb_filterpolicy_destroyF

c_rocksdb_filterpolicy_create_bloom_full :: Int -> IO FilterPolicyFPtr
c_rocksdb_filterpolicy_create_bloom_full n =
    {#call rocksdb_filterpolicy_create_bloom_full #} (cIntConv n) >>= newForeignPtr c_rocksdb_filterpolicy_destroyF

{#fun rocksdb_filterpolicy_destroy as c_rocksdb_filterpolicy_destroy
    {`FilterPolicyFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_filterpolicy_destroy"
    c_rocksdb_filterpolicy_destroyF :: FunPtr (FilterPolicyPtr -> IO ())

----------------------------------------------
-- Merge Operator
----------------------------------------------

type CFullMergeCb = OpaquePtr
                 -> CString      -- ^ key
                 -> CSize        -- ^ key_length
                 -> CString      -- ^ existing_value
                 -> CSize        -- ^ existing_value_length
                 -> Ptr CString  -- ^ operands_list
                 -> Ptr CSize    -- ^ operands_list_length
                 -> CInt         -- ^ num_operands
                 -> Ptr CUChar   -- ^ success
                 -> Ptr CSize    -- ^ new_value_length
                 -> IO CString

foreign import ccall "wrapper" mkFullMergeCb :: CFullMergeCb -> IO (FunPtr CFullMergeCb)

type CPartialMergeCb = OpaquePtr
                    -> CString      -- ^ key
                    -> CSize        -- ^ key_length
                    -> Ptr CString  -- ^ operands_list
                    -> Ptr CSize    -- ^ operands_list_length
                    -> CInt         -- ^ num_operands
                    -> Ptr CUChar   -- ^ success
                    -> Ptr CSize    -- ^ new_value_length
                    -> IO CString

foreign import ccall "wrapper" mkPatrialMergeCb :: CPartialMergeCb -> IO (FunPtr CPartialMergeCb)

type CDeleteValueCb = OpaquePtr
                   -> CString    -- ^ value
                   -> CSize      -- ^ value_length

foreign import ccall "wrapper" mkMergeDeleteCb :: CDeleteValueCb -> IO (FunPtr CDeleteValueCb)

foreign import ccall safe "rocksdb/c.h rocksdb_mergeoperator_create"
    c_rocksdb_mergeoperator_create :: OpaquePtr
                                   -> FunPtr Destructor
                                   -> FunPtr CFullMergeCb
                                   -> FunPtr CPartialMergeCb
                                   -> FunPtr CDeleteValueCb
                                   -> FunPtr NameFun
                                   -> IO MergeOperatorPtr

{#fun rocksdb_mergeoperator_destroy as c_rocksdb_mergeoperator_destroy
    {`MergeOperatorFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_mergeoperator_destroy"
    c_rocksdb_mergeoperator_destroyF :: FunPtr (MergeOperatorPtr -> IO ())

----------------------------------------------
-- Read options
----------------------------------------------

c_rocksdb_readoptions_create :: IO ReadOptionsFPtr
c_rocksdb_readoptions_create =
    {#call rocksdb_readoptions_create #} >>= newForeignPtr c_rocksdb_readoptions_destroyF

{#fun rocksdb_readoptions_destroy as c_rocksdb_readoptions_destroy
    {`ReadOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_readoptions_destroy"
    c_rocksdb_readoptions_destroyF :: FunPtr (ReadOptionsPtr -> IO ())

{#fun rocksdb_readoptions_set_verify_checksums as c_rocksdb_readoptions_set_verify_checksums
    {`ReadOptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_readoptions_set_fill_cache as c_rocksdb_readoptions_set_fill_cache
    {`ReadOptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_readoptions_set_snapshot as c_rocksdb_readoptions_set_snapshot
    {`ReadOptionsFPtr', `SnapshotFPtr'} -> `()' #}

{#fun rocksdb_readoptions_set_iterate_upper_bound as c_rocksdb_readoptions_set_iterate_upper_bound
    {`ReadOptionsFPtr', bsToCStringLen* `ByteString'&} -> `()' #}

{#fun rocksdb_readoptions_set_read_tier as c_rocksdb_readoptions_set_read_tier
    {`ReadOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_readoptions_set_tailing as c_rocksdb_readoptions_set_tailing
    {`ReadOptionsFPtr', boolToNum `Bool'} -> `()' #}

----------------------------------------------
-- Write options
----------------------------------------------

c_rocksdb_writeoptions_create :: IO WriteOptionsFPtr
c_rocksdb_writeoptions_create =
    {#call rocksdb_writeoptions_create #} >>= newForeignPtr c_rocksdb_writeoptions_destroyF

{#fun rocksdb_writeoptions_destroy as c_rocksdb_writeoptions_destroy
    {`WriteOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_writeoptions_destroy"
    c_rocksdb_writeoptions_destroyF :: FunPtr (WriteOptionsPtr -> IO ())

{#fun rocksdb_writeoptions_set_sync as c_rocksdb_writeoptions_set_sync
    {`WriteOptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_writeoptions_disable_WAL as c_rocksdb_writeoptions_disable_WAL
    {`WriteOptionsFPtr', boolToNum `Bool'} -> `()' #}

----------------------------------------------
-- Flush options
----------------------------------------------

c_rocksdb_flushoptions_create :: IO FlushOptionsFPtr
c_rocksdb_flushoptions_create =
    {#call rocksdb_flushoptions_create #} >>= newForeignPtr c_rocksdb_flushoptions_destroyF

{#fun rocksdb_flushoptions_destroy as c_rocksdb_flushoptions_destroy
    {`FlushOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_flushoptions_destroy"
    c_rocksdb_flushoptions_destroyF :: FunPtr (FlushOptionsPtr -> IO ())

{#fun rocksdb_flushoptions_set_wait as c_rocksdb_flushoptions_set_wait
    {`FlushOptionsFPtr', boolToNum `Bool'} -> `()' #}

----------------------------------------------
-- Cache
----------------------------------------------

c_rocksdb_cache_create_lru :: CSize -> IO CacheFPtr
c_rocksdb_cache_create_lru sz =
    {#call rocksdb_cache_create_lru #} (cIntConv sz) >>= newForeignPtr c_rocksdb_cache_destroyF

{#fun rocksdb_cache_destroy as c_rocksdb_cache_destroy
    {`CacheFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_cache_destroy"
    c_rocksdb_cache_destroyF :: FunPtr (CachePtr -> IO ())

----------------------------------------------
-- Env
----------------------------------------------

c_rocksdb_create_default_env :: IO EnvFPtr
c_rocksdb_create_default_env =
    {#call rocksdb_create_default_env #} >>= newForeignPtr c_rocksdb_env_destroyF

{#fun rocksdb_env_destroy as c_rocksdb_env_destroy
    {`EnvFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_env_destroy"
    c_rocksdb_env_destroyF :: FunPtr (EnvPtr -> IO ())

{#fun rocksdb_env_set_background_threads as c_rocksdb_env_set_background_threads
    {`EnvFPtr', `Int'} -> `()' #}

{#fun rocksdb_env_set_high_priority_background_threads as c_rocksdb_env_set_high_priority_background_threads
    {`EnvFPtr', `Int'} -> `()' #}

{#fun rocksdb_env_join_all_threads as c_rocksdb_env_join_all_threads
    {`EnvFPtr'} -> `()' #}

----------------------------------------------
-- SliceTransform
----------------------------------------------

type CTransformCb = OpaquePtr
                 -> CString     -- ^ key
                 -> CSize       -- ^ length
                 -> Ptr CSize   -- ^ dst_length
                 -> IO CString

type CInDomainCb = OpaquePtr -> CString -> CSize -> IO CUChar
type CInRangeCb  = OpaquePtr -> CString -> CSize -> IO CUChar

foreign import ccall "wrapper" mkTransformCb :: CTransformCb -> IO (FunPtr CTransformCb)

foreign import ccall "wrapper" mkInDomainCb :: CInDomainCb -> IO (FunPtr CInDomainCb)

foreign import ccall "wrapper" mkInRangeCb :: CInRangeCb -> IO (FunPtr CInRangeCb)

foreign import ccall safe "rocksdb/c.h rocksdb_mergeoperator_create"
    rocksdb_slicetransform_create :: OpaquePtr
                                  -> FunPtr Destructor
                                  -> FunPtr CTransformCb
                                  -> FunPtr CInDomainCb
                                  -> FunPtr CInRangeCb
                                  -> FunPtr NameFun
                                  -> IO SliceTransformPtr

c_rocksdb_slicetransform_create_fixed_prefix :: CSize -> IO SliceTransformFPtr
c_rocksdb_slicetransform_create_fixed_prefix sz =
    {#call rocksdb_slicetransform_create_fixed_prefix #} (cIntConv sz) >>= newForeignPtr c_rocksdb_slicetransform_destroyF

c_rocksdb_slicetransform_create_noop :: IO SliceTransformFPtr
c_rocksdb_slicetransform_create_noop =
    {#call rocksdb_slicetransform_create_noop #} >>= newForeignPtr c_rocksdb_slicetransform_destroyF

{#fun rocksdb_slicetransform_destroy as c_rocksdb_slicetransform_destroy
    {`SliceTransformFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_slicetransform_destroy"
    c_rocksdb_slicetransform_destroyF :: FunPtr (SliceTransformPtr -> IO ())

----------------------------------------------
-- Live files
----------------------------------------------

{# fun rocksdb_livefiles_count as c_rocksdb_livefiles_count
    {`LiveFilesFPtr'} -> `Int' #}

{# fun rocksdb_livefiles_name as c_rocksdb_livefiles_name
    {`LiveFilesFPtr', `Int'} -> `String' #}

{# fun rocksdb_livefiles_level as c_rocksdb_livefiles_level
    {`LiveFilesFPtr', `Int'} -> `Int' #}

{# fun rocksdb_livefiles_size as c_rocksdb_livefiles_size
    {`LiveFilesFPtr', `Int'} -> `CSize' cIntConv #}

{#fun rocksdb_livefiles_destroy as c_rocksdb_livefiles_destroy
    {`LiveFilesFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_livefiles_destroy"
    c_rocksdb_livefiles_destroyF :: FunPtr (LiveFilesPtr -> IO ())

{# fun rocksdb_livefiles_smallestkey as c_rocksdb_livefiles_smallestkey
    {`LiveFilesFPtr', `Int', alloca- `Int' peekIntConv*} -> `String' #}

{# fun rocksdb_livefiles_largestkey as c_rocksdb_livefiles_largestkey
    {`LiveFilesFPtr', `Int', alloca- `Int' peekIntConv*} -> `String' #}

----------------------------------------------
-- Live files
----------------------------------------------

-- {#fun rocksdb_get_options_from_string as c_rocksdb_get_options_from_string
--     {`OptionsFPtr', `String', `OptionsFPtr',
--     rocksdb_options_t* new_options, char** errptr);

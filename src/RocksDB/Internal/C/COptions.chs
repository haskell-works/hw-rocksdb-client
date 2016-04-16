{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module RocksDB.Internal.C.COptions where

import Foreign
import Foreign.C.Types
{#import RocksDB.Internal.C.CTypes#}

import RocksDB.Internal.C.C2HS


#include <rocksdb/c.h>

----------------------------------------------
-- Options
----------------------------------------------

{#enum rocksdb_no_compression as Compression 
    {underscoreToCase} with prefix = "Rocksdb" deriving (Eq, Ord, Show) #}

{#enum rocksdb_level_compaction as Compaction 
    {underscoreToCase} with prefix = "Rocksdb" deriving (Eq, Ord, Show)#}

c_rocksdb_options_create :: IO OptionsFPtr
c_rocksdb_options_create =
    {#call rocksdb_options_create #} >>= newForeignPtr c_rocksdb_options_destroyF

{#fun rocksdb_options_destroy as c_rocksdb_options_destroy 
    {`OptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_options_destroy"
    c_rocksdb_options_destroyF :: FunPtr (OptionsPtr -> IO ())

{#fun rocksdb_options_increase_parallelism as c_rocksdb_options_increase_parallelism
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_optimize_for_point_lookup as c_rocksdb_options_optimize_for_point_lookup
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_optimize_level_style_compaction as c_rocksdb_options_optimize_level_style_compaction
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_optimize_universal_style_compaction as c_rocksdb_options_optimize_universal_style_compaction
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_compaction_filter as c_rocksdb_options_set_compaction_filter
    {`OptionsFPtr', `CompactionFilterFPtr'} -> `()' #}

{#fun rocksdb_options_set_compaction_filter_factory as c_rocksdb_options_set_compaction_filter_factory
    {`OptionsFPtr', `CompactionFilterFactoryFPtr'} -> `()' #}

{#fun rocksdb_options_set_comparator as c_rocksdb_options_set_comparator
    {`OptionsFPtr', `ComparatorFPtr'} -> `()' #}

{#fun rocksdb_options_set_merge_operator as c_rocksdb_options_set_merge_operator
    {`OptionsFPtr', `MergeOperatorFPtr'} -> `()' #}

{#fun rocksdb_options_set_uint64add_merge_operator as c_rocksdb_options_set_uint64add_merge_operator
    {`OptionsFPtr'} -> `()' #}

{#fun rocksdb_options_set_compression_per_level as c_rocksdb_options_set_compression_per_level
    {`OptionsFPtr', castPtr `Ptr Int', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_create_if_missing as c_rocksdb_options_set_create_if_missing
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_create_missing_column_families as c_rocksdb_options_set_create_missing_column_families
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_error_if_exists as c_rocksdb_options_set_error_if_exists
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_paranoid_checks as c_rocksdb_options_set_paranoid_checks
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_env as c_rocksdb_options_set_env
    {`OptionsFPtr', `EnvFPtr'} -> `()' #}

{#fun rocksdb_options_set_info_log as c_rocksdb_options_set_info_log
    {`OptionsFPtr', `LoggerFPtr'} -> `()' #}

{#fun rocksdb_options_set_info_log_level as c_rocksdb_options_set_info_log_level
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_write_buffer_size as c_rocksdb_options_set_write_buffer_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_max_open_files as c_rocksdb_options_set_max_open_files
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_total_wal_size as c_rocksdb_options_set_max_total_wal_size
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_compression_options as c_rocksdb_options_set_compression_options
    {`OptionsFPtr', `Int', `Int', `Int'} -> `()' #}

{#fun rocksdb_options_set_prefix_extractor as c_rocksdb_options_set_prefix_extractor
    {`OptionsFPtr', `SliceTransformFPtr'} -> `()' #}

{#fun rocksdb_options_set_num_levels as c_rocksdb_options_set_num_levels
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_level0_file_num_compaction_trigger as c_rocksdb_options_set_level0_file_num_compaction_trigger
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_level0_slowdown_writes_trigger as c_rocksdb_options_set_level0_slowdown_writes_trigger
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_level0_stop_writes_trigger as c_rocksdb_options_set_level0_stop_writes_trigger
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_mem_compaction_level as c_rocksdb_options_set_max_mem_compaction_level
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_target_file_size_base as c_rocksdb_options_set_target_file_size_base
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_target_file_size_multiplier as c_rocksdb_options_set_target_file_size_multiplier
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_bytes_for_level_base as c_rocksdb_options_set_max_bytes_for_level_base
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_max_bytes_for_level_multiplier as c_rocksdb_options_set_max_bytes_for_level_multiplier
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_expanded_compaction_factor as c_rocksdb_options_set_expanded_compaction_factor
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_grandparent_overlap_factor as c_rocksdb_options_set_max_grandparent_overlap_factor
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_bytes_for_level_multiplier_additional as c_rocksdb_options_set_max_bytes_for_level_multiplier_additional
    {`OptionsFPtr', castPtr `Ptr CInt', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_enable_statistics as c_rocksdb_options_enable_statistics
    {`OptionsFPtr'} -> `()' #}


{#fun rocksdb_options_set_max_write_buffer_number as c_rocksdb_options_set_max_write_buffer_number
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_min_write_buffer_number_to_merge as c_rocksdb_options_set_min_write_buffer_number_to_merge
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_write_buffer_number_to_maintain as c_rocksdb_options_set_max_write_buffer_number_to_maintain
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_background_compactions as c_rocksdb_options_set_max_background_compactions
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_background_flushes as c_rocksdb_options_set_max_background_flushes
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_max_log_file_size as c_rocksdb_options_set_max_log_file_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_log_file_time_to_roll as c_rocksdb_options_set_log_file_time_to_roll
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_keep_log_file_num as c_rocksdb_options_set_keep_log_file_num
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_recycle_log_file_num as c_rocksdb_options_set_recycle_log_file_num
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_soft_rate_limit as c_rocksdb_options_set_soft_rate_limit
    {`OptionsFPtr', `Double'} -> `()' #}

{#fun rocksdb_options_set_hard_rate_limit as c_rocksdb_options_set_hard_rate_limit
    {`OptionsFPtr', `Double'} -> `()' #}

{#fun rocksdb_options_set_rate_limit_delay_max_milliseconds as c_rocksdb_options_set_rate_limit_delay_max_milliseconds
    {`OptionsFPtr', cIntConv `CUInt'} -> `()' #}

{#fun rocksdb_options_set_max_manifest_file_size as c_rocksdb_options_set_max_manifest_file_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_table_cache_numshardbits as c_rocksdb_options_set_table_cache_numshardbits
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_table_cache_remove_scan_count_limit as c_rocksdb_options_set_table_cache_remove_scan_count_limit
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_arena_block_size as c_rocksdb_options_set_arena_block_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_use_fsync as c_rocksdb_options_set_use_fsync
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_db_log_dir as c_rocksdb_options_set_db_log_dir
    {`OptionsFPtr', `String'} -> `()' #}

{#fun rocksdb_options_set_wal_dir as c_rocksdb_options_set_wal_dir
    {`OptionsFPtr', `String'} -> `()' #}

{#fun rocksdb_options_set_WAL_ttl_seconds as c_rocksdb_options_set_WAL_ttl_seconds
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_WAL_size_limit_MB as c_rocksdb_options_set_WAL_size_limit_MB
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_manifest_preallocation_size as c_rocksdb_options_set_manifest_preallocation_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_purge_redundant_kvs_while_flush as c_rocksdb_options_set_purge_redundant_kvs_while_flush
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_allow_os_buffer as c_rocksdb_options_set_allow_os_buffer
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_allow_mmap_reads as c_rocksdb_options_set_allow_mmap_reads
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_allow_mmap_writes as c_rocksdb_options_set_allow_mmap_writes
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_is_fd_close_on_exec as c_rocksdb_options_set_is_fd_close_on_exec
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_skip_log_error_on_recovery as c_rocksdb_options_set_skip_log_error_on_recovery
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_stats_dump_period_sec as c_rocksdb_options_set_stats_dump_period_sec
    {`OptionsFPtr', cIntConv `CUInt'} -> `()' #}

{#fun rocksdb_options_set_advise_random_on_open as c_rocksdb_options_set_advise_random_on_open
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_access_hint_on_compaction_start as c_rocksdb_options_set_access_hint_on_compaction_start
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_use_adaptive_mutex as c_rocksdb_options_set_use_adaptive_mutex
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_bytes_per_sync as c_rocksdb_options_set_bytes_per_sync
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_verify_checksums_in_compaction as c_rocksdb_options_set_verify_checksums_in_compaction
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_filter_deletes as c_rocksdb_options_set_filter_deletes
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_max_sequential_skip_in_iterations as c_rocksdb_options_set_max_sequential_skip_in_iterations
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_disable_data_sync as c_rocksdb_options_set_disable_data_sync
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_disable_auto_compactions as c_rocksdb_options_set_disable_auto_compactions
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_delete_obsolete_files_period_micros as c_rocksdb_options_set_delete_obsolete_files_period_micros
    {`OptionsFPtr', cIntConv `CUInt64T'} -> `()' #}

{#fun rocksdb_options_set_source_compaction_factor as c_rocksdb_options_set_source_compaction_factor
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_prepare_for_bulk_load as c_rocksdb_options_prepare_for_bulk_load
    {`OptionsFPtr'} -> `()' #}

{#fun rocksdb_options_set_memtable_vector_rep as c_rocksdb_options_set_memtable_vector_rep
    {`OptionsFPtr'} -> `()' #}

{#fun rocksdb_options_set_hash_skip_list_rep as c_rocksdb_options_set_hash_skip_list_rep
    {`OptionsFPtr', cIntConv `CSize', cIntConv `CInt32T', cIntConv `CInt32T'} -> `()' #}

{#fun rocksdb_options_set_hash_link_list_rep as c_rocksdb_options_set_hash_link_list_rep
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_plain_table_factory as c_rocksdb_options_set_plain_table_factory
    {`OptionsFPtr', cIntConv `CUInt32T', `Int', `Double', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_min_level_to_compress as c_rocksdb_options_set_min_level_to_compress
    {`OptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_options_set_memtable_prefix_bloom_bits as c_rocksdb_options_set_memtable_prefix_bloom_bits
    {`OptionsFPtr', cIntConv `CUInt32T'} -> `()' #}

{#fun rocksdb_options_set_memtable_prefix_bloom_probes as c_rocksdb_options_set_memtable_prefix_bloom_probes
    {`OptionsFPtr', cIntConv `CUInt32T'} -> `()' #}

{#fun rocksdb_options_set_memtable_prefix_bloom_huge_page_tlb_size as c_rocksdb_options_set_memtable_prefix_bloom_huge_page_tlb_size
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_max_successive_merges as c_rocksdb_options_set_max_successive_merges
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_min_partial_merge_operands as c_rocksdb_options_set_min_partial_merge_operands
    {`OptionsFPtr', cIntConv `CUInt32T'} -> `()' #}

{#fun rocksdb_options_set_bloom_locality as c_rocksdb_options_set_bloom_locality
    {`OptionsFPtr', cIntConv `CUInt32T'} -> `()' #}

{#fun rocksdb_options_set_inplace_update_support as c_rocksdb_options_set_inplace_update_support
    {`OptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_options_set_inplace_update_num_locks as c_rocksdb_options_set_inplace_update_num_locks
    {`OptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_options_set_compression as c_rocksdb_options_set_compression
    {`OptionsFPtr', enumToCInt `Compression'} -> `()' #}

{#fun rocksdb_options_set_compaction_style as c_rocksdb_options_set_compaction_style
    {`OptionsFPtr', enumToCInt `Compaction'} -> `()' #}

{#fun rocksdb_options_set_universal_compaction_options as c_rocksdb_options_set_universal_compaction_options
    {`OptionsFPtr', `UniversalCompactionOptionsFPtr'} -> `()' #}

{#fun rocksdb_options_set_fifo_compaction_options as c_rocksdb_options_set_fifo_compaction_options
    {`OptionsFPtr', `FifoCompactionOptionsFPtr'} -> `()' #}

-- returns a pointer to a malloc()-ed, null terminated string
{#fun rocksdb_options_statistics_get_string as c_rocksdb_options_statistics_get_string
    {`OptionsFPtr'} -> `String' #}

----------------------------------------------
-- Block based table options
----------------------------------------------

{#enum rocksdb_block_based_table_index_type_binary_search as BlockBaseTableIndexSearchType 
    {underscoreToCase} with prefix = "Rocksdb" deriving (Eq, Ord, Show) #}

c_rocksdb_block_based_options_create :: IO BlockBasedTableOptionsFPtr
c_rocksdb_block_based_options_create =
    {#call rocksdb_block_based_options_create #} >>= newForeignPtr c_rocksdb_block_based_options_destroyF
    
{#fun rocksdb_block_based_options_destroy as c_rocksdb_block_based_options_destroy 
    {`BlockBasedTableOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_block_based_options_destroy"
    c_rocksdb_block_based_options_destroyF :: FunPtr (BlockBasedTableOptionsPtr -> IO ())

{#fun rocksdb_block_based_options_set_block_size as c_rocksdb_block_based_options_set_block_size 
    {`BlockBasedTableOptionsFPtr', cIntConv `CSize'} -> `()' #}

{#fun rocksdb_block_based_options_set_block_size_deviation as c_rocksdb_block_based_options_set_block_size_deviation 
    {`BlockBasedTableOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_block_based_options_set_block_restart_interval as c_rocksdb_block_based_options_set_block_restart_interval 
    {`BlockBasedTableOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_block_based_options_set_filter_policy as c_rocksdb_block_based_options_set_filter_policy 
    {`BlockBasedTableOptionsFPtr', `FilterPolicyFPtr'} -> `()'  #}

{#fun rocksdb_block_based_options_set_no_block_cache as c_rocksdb_block_based_options_set_no_block_cache 
    {`BlockBasedTableOptionsFPtr', boolToNum `Bool'} -> `()'  #}

{#fun rocksdb_block_based_options_set_block_cache as c_rocksdb_block_based_options_set_block_cache 
    {`BlockBasedTableOptionsFPtr', `CacheFPtr'} -> `()'  #}

{#fun rocksdb_block_based_options_set_block_cache_compressed as c_rocksdb_block_based_options_set_block_cache_compressed 
    {`BlockBasedTableOptionsFPtr', `CacheFPtr'} -> `()'  #}

{#fun rocksdb_block_based_options_set_whole_key_filtering as c_rocksdb_block_based_options_set_whole_key_filtering 
    {`BlockBasedTableOptionsFPtr', boolToNum `Bool'} -> `()'  #}

{#fun rocksdb_block_based_options_set_format_version as c_rocksdb_block_based_options_set_format_version 
    {`BlockBasedTableOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_block_based_options_set_index_type as c_rocksdb_block_based_options_set_index_type 
    {`BlockBasedTableOptionsFPtr',  enumToCInt `BlockBaseTableIndexSearchType'} -> `()' #}

{#fun rocksdb_block_based_options_set_hash_index_allow_collision as c_rocksdb_block_based_options_set_hash_index_allow_collision 
    {`BlockBasedTableOptionsFPtr', boolToNum `Bool'} -> `()'  #}

{#fun rocksdb_block_based_options_set_cache_index_and_filter_blocks as c_rocksdb_block_based_options_set_cache_index_and_filter_blocks 
    {`BlockBasedTableOptionsFPtr', boolToNum `Bool'} -> `()'  #}

{#fun rocksdb_block_based_options_set_skip_table_builder_flush as c_rocksdb_block_based_options_set_skip_table_builder_flush 
    {`BlockBasedTableOptionsFPtr', boolToNum `Bool'} -> `()'  #}

{#fun rocksdb_options_set_block_based_table_factory as c_rocksdb_options_set_block_based_table_factory 
    {`OptionsFPtr', `BlockBasedTableOptionsFPtr'} -> `()' #}

----------------------------------------------
-- Cuckoo table options
----------------------------------------------

c_rocksdb_cuckoo_options_create :: IO CuckooTableOptionsFPtr
c_rocksdb_cuckoo_options_create =
    {#call rocksdb_cuckoo_options_create #} >>= newForeignPtr c_rocksdb_cuckoo_options_destroyF

{#fun rocksdb_cuckoo_options_destroy as c_rocksdb_cuckoo_options_destroy 
    {`CuckooTableOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_cuckoo_options_destroy"
    c_rocksdb_cuckoo_options_destroyF :: FunPtr (CuckooTableOptionsPtr -> IO ())

{#fun rocksdb_cuckoo_options_set_hash_ratio as c_rocksdb_cuckoo_options_set_hash_ratio
    {`CuckooTableOptionsFPtr', `Double'} -> `()' #}

{#fun rocksdb_cuckoo_options_set_max_search_depth as c_rocksdb_cuckoo_options_set_max_search_depth
    {`CuckooTableOptionsFPtr', `Word32'} -> `()' #}

{#fun rocksdb_cuckoo_options_set_cuckoo_block_size as c_rocksdb_cuckoo_options_set_cuckoo_block_size
    {`CuckooTableOptionsFPtr', `Word32'} -> `()' #}

{#fun rocksdb_cuckoo_options_set_identity_as_first_hash as c_rocksdb_cuckoo_options_set_identity_as_first_hash
    {`CuckooTableOptionsFPtr', boolToNum `Bool'} -> `()' #}

{#fun rocksdb_cuckoo_options_set_use_module_hash as c_rocksdb_cuckoo_options_set_use_module_hash
    {`CuckooTableOptionsFPtr', boolToNum `Bool'}  -> `()' #}

{#fun rocksdb_options_set_cuckoo_table_factory as c_rocksdb_options_set_cuckoo_table_factory
   {`OptionsFPtr', `CuckooTableOptionsFPtr'} -> `()' #}

----------------------------------------------
-- Universal Compaction options
----------------------------------------------

{#enum rocksdb_similar_size_compaction_stop_style as CompactionStopStyle 
    {underscoreToCase} with prefix = "Rocksdb" deriving (Eq, Ord, Show) #}

c_rocksdb_universal_compaction_options_create :: IO UniversalCompactionOptionsFPtr
c_rocksdb_universal_compaction_options_create =
    {#call rocksdb_universal_compaction_options_create #} >>= newForeignPtr c_rocksdb_universal_compaction_options_destroyF

{#fun rocksdb_universal_compaction_options_destroy as c_rocksdb_universal_compaction_options_destroy 
    {`UniversalCompactionOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_universal_compaction_options_destroy"
    c_rocksdb_universal_compaction_options_destroyF :: FunPtr (UniversalCompactionOptionsPtr -> IO ())

{#fun rocksdb_universal_compaction_options_set_size_ratio as c_rocksdb_universal_compaction_options_set_size_ratio
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_universal_compaction_options_set_min_merge_width as c_rocksdb_universal_compaction_options_set_min_merge_width
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_universal_compaction_options_set_max_merge_width as c_rocksdb_universal_compaction_options_set_max_merge_width
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_universal_compaction_options_set_max_size_amplification_percent as c_rocksdb_universal_compaction_options_set_max_size_amplification_percent
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_universal_compaction_options_set_compression_size_percent as c_rocksdb_universal_compaction_options_set_compression_size_percent
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

{#fun rocksdb_universal_compaction_options_set_stop_style as c_rocksdb_universal_compaction_options_set_stop_style
    {`UniversalCompactionOptionsFPtr', `Int'} -> `()' #}

----------------------------------------------
-- Universal Fifo options
----------------------------------------------

c_rocksdb_fifo_compaction_options_create :: IO FifoCompactionOptionsFPtr
c_rocksdb_fifo_compaction_options_create =
    {#call rocksdb_fifo_compaction_options_create #} >>= newForeignPtr c_rocksdb_fifo_compaction_options_destroyF
    
{#fun rocksdb_fifo_compaction_options_destroy as c_rocksdb_fifo_compaction_options_destroy 
    {`FifoCompactionOptionsFPtr'} -> `()' #}

foreign import ccall safe "rocksdb/c.h &rocksdb_fifo_compaction_options_destroy"
    c_rocksdb_fifo_compaction_options_destroyF :: FunPtr (FifoCompactionOptionsPtr -> IO ())

{#fun rocksdb_fifo_compaction_options_set_max_table_files_size as c_rocksdb_fifo_compaction_options_set_max_table_files_size
    {`FifoCompactionOptionsFPtr', cIntConv `CUInt64T'} -> `()' #}


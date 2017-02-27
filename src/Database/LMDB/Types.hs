
{-# LANGUAGE DataKinds
           , KindSignatures
           #-}

module Database.LMDB.Types where

import Data.Default.Class

import Database.LMDB.Internal.Raw

data Mode = ReadOnly
          | ReadWrite
          deriving ( Eq
                   , Ord
                   , Read
                   , Show
                   )

class KnownMode (m :: Mode) where
    mode :: f m -> Mode

instance KnownMode 'ReadOnly where
    mode _ = ReadOnly

instance KnownMode 'ReadWrite where
    mode _ = ReadWrite

newtype Env (m :: Mode) = Env { getEnv :: MDB_env }

data EnvConfig = EnvConfig {
    fixMmapAddr    :: Bool
  , noSubDir       :: Bool
  , noCommitSync   :: Bool
  , noMetaPageSync :: Bool
    -- | Breaks nested transactions.
  , writeMap       :: Bool
  , asyncSync      :: Bool
    -- | Breaks everything.
  , noLock         :: Bool
  , noReadAhead    :: Bool
  , noMemInit      :: Bool
  , maxReaders     :: Int
  , maxDBs         :: Int
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )

instance Default EnvConfig where
    def = EnvConfig False False False False False False False False False 128 16

envConfigToFlags :: EnvConfig -> [MDB_EnvFlag]
envConfigToFlags c = let iff False _ = []
                         iff True  x = [x]
                     in concat [ iff (fixMmapAddr c) MDB_FIXEDMAP
                               , iff (noSubDir c) MDB_NOSUBDIR
                               , iff (noCommitSync c) MDB_NOSYNC
                               , iff (writeMap c) MDB_WRITEMAP
                               , iff (asyncSync c) MDB_MAPASYNC
                               , iff (noLock c) MDB_NOLOCK
                               , iff (noReadAhead c) MDB_NORDAHEAD
                               , iff (noMemInit c) MDB_NOMEMINIT
                               ]

newtype DB k v = DB { getDB :: MDB_dbi' }

data DBName = Main
           | Named String
           deriving ( Eq
                    , Ord
                    , Read
                    , Show
                    )

dbNameToMaybe :: DBName -> Maybe String
dbNameToMaybe Main      = Nothing
dbNameToMaybe (Named n) = Just n

data DBConfig = DBConfig {
   revKey     :: Bool
 , dupSort    :: Bool
   -- | Don't use this yet.
 , integerKey :: Bool
 , dupFixed   :: Bool
 , integerDup :: Bool
 , reverseDup :: Bool
 , create     :: Bool
 } deriving ( Eq
            , Ord
            , Read
            , Show
            )

instance Default DBConfig where
   def = DBConfig False False False False False False True

dbConfigToFlags :: DBConfig -> [MDB_DbFlag]
dbConfigToFlags c = let iff bool  x = [x | bool]
                   in concat [ iff (revKey c) MDB_REVERSEKEY
                             , iff (dupSort c) MDB_DUPSORT
                             , iff (integerKey c) MDB_INTEGERKEY
                             , iff (dupFixed c) MDB_DUPFIXED
                             , iff (integerDup c) MDB_INTEGERDUP
                             , iff (reverseDup c) MDB_REVERSEDUP
                             , iff (create c) MDB_CREATE
                             ]

--
initReadOnlyEnv :: EnvConfig
                -> FilePath -- ^ Environment data directory.
                -> Int      -- ^ Map size.
                -> IO (Env 'ReadOnly)
initReadOnlyEnv c fp ms = do
    e   <- mdb_env_create
    mdb_env_set_mapsize e ms
    mdb_env_set_maxreaders e (maxReaders c)
    mdb_env_set_maxdbs e (maxDBs c)
    mdb_env_open e fp (MDB_RDONLY : envConfigToFlags c)
    pure (Env e)

initReadWriteEnv :: EnvConfig
                 -> FilePath -- ^ Environment data directory.
                 -> Int      -- ^ Map size.
                 -> IO (Env 'ReadWrite)
initReadWriteEnv c fp ms = do
    e   <- mdb_env_create
    mdb_env_set_mapsize e ms
    mdb_env_set_maxreaders e (maxReaders c)
    mdb_env_set_maxdbs e (maxDBs c)
    mdb_env_open e fp (envConfigToFlags c)
    pure (Env e)

-- | TODO: add force.
syncEnv :: Env m -> IO ()
syncEnv (Env e) = mdb_env_sync e

closeEnv :: Env m -> IO ()
closeEnv (Env e) = mdb_env_close e

--
openDB :: Env m -> DBName -> DBConfig -> IO (DB k v)
openDB (Env e) n c = do
    tx <- mdb_txn_begin e Nothing False
    db <- mdb_dbi_open' tx (dbNameToMaybe n) (dbConfigToFlags c)
    mdb_txn_commit tx
    pure (DB db)

closeDB :: Env m -> DB k v -> IO ()
closeDB (Env e) (DB d) = mdb_dbi_close' e d

{-# LANGUAGE DataKinds
           , KindSignatures
           #-}

module Database.LMDB (
    Mode(..)
  , KnownMode(..)
    -- * Environments
  , Env(..)
  , EnvConfig(..)
  , initReadOnlyEnv
  , initReadWriteEnv
  , syncEnv
  , closeEnv
    -- * Databases
  , DB(..)
  , DBName(..)
  , DBConfig(..)
  , openDB
  , closeDB
  , def
  , withEnv
  , withReadOnlyEnv
  , withDB
  ) where

import Control.Monad.Catch
import Control.Monad.Reader

import Data.Default.Class

import Database.LMDB.Types

-- * Continuation-style access to environment (read-write).
withEnv
    :: MonadIO m
    => MonadMask m
    => EnvConfig
    -> FilePath
    -> Int
    -> ReaderT (Env 'ReadWrite) m a
    -> m a
withEnv cfg path size action = do
    bracket
        (liftIO $ initReadWriteEnv cfg path size)
        (liftIO . closeEnv)
        (runReaderT action)

-- * Continuation-style access to environment (read-write).
withReadOnlyEnv
    :: MonadIO m
    => MonadMask m
    => EnvConfig
    -> FilePath
    -> Int
    -> ReaderT (Env 'ReadOnly) m a
    -> m a
withReadOnlyEnv cfg path size action = do
    bracket
        (liftIO $ initReadOnlyEnv cfg path size)
        (liftIO . closeEnv)
        (runReaderT action)

withDB
    :: MonadIO m
    => MonadMask m
    => DBConfig
    -> DBName
    -> Env mode
    -> ReaderT (DB k v) m a
    -> m a
withDB c n e action = do
    bracket
        (liftIO $ openDB e n c)
        (liftIO . closeDB e)
        (runReaderT action)

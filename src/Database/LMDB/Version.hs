{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           #-}

module Database.LMDB.Version (
    LMDBVersion(..)
  , compiledVersion
  , dynLinkedVersion
  ) where

import Control.DeepSeq

import GHC.Generics

import Database.LMDB.Internal.Raw

data LMDBVersion = LMDBVersion {
    major :: !Int
  , minor :: !Int
  , patch :: !Int
  , text  :: String
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

v2v :: LMDB_Version -> LMDBVersion
v2v (LMDB_Version mj mn p t) = LMDBVersion mj mn p t

compiledVersion :: LMDBVersion
compiledVersion = v2v lmdb_version

dynLinkedVersion :: IO LMDBVersion
dynLinkedVersion = v2v <$> lmdb_dyn_version

{-# LANGUAGE DeriveAnyClass
           , DeriveDataTypeable
           , DeriveGeneric
           #-}

module Database.LMDB.Error where

import Control.DeepSeq
import Control.Exception

import Data.Typeable

import GHC.Generics

import Database.LMDB.Internal.Raw

data LMDBException = KeyExists
                   | KeyNotFound
                     -- | Probably corruption.
                   | PageNotFound
                   | PageCorrupted
                   | Panic
                   | VersionMismatch
                   | FileInvalid
                   | MapFull
                     -- | Maximum number of databases reached.
                   | MaxDBs
                   | MaxReaders
                     -- | Transaction too large.
                   | MaxDirtyPages
                     -- | Cursor operation too long.
                   | MaxCursorStack
                   | PageFull
                   | MapResized
                     -- | Operation not supported on DB type.
                   | UnsupportedOp
                     -- | Reader lock table corrupted.
                   | InvalidLockTable
                   | TxFault
                     -- | Key size too large.
                   | BadValueSize
                     -- | Database changed unexpectedly, likely out of bounds
                     --   memory mutation.
                   | OtherError LMDB_Error
                   deriving ( Eq
                            , Ord
                            , Read
                            , Show
                            , Generic
                            , NFData
                            , Typeable
                            )

instance Exception LMDBException

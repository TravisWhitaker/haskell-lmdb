{-# LANGUAGE DefaultSignatures #-}

module Database.LMDB.Stowable (
    Stowable(..)
  ) where

import Control.DeepSeq

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Database.LMDB.Internal.Raw

class NFData a => Stowable a where
    get :: MDB_val -> IO a
    default get :: Storable a => MDB_val -> IO a
    get (MDB_val _ p) = force <$> peek (castPtr p)
    put :: a -> (MDB_val -> IO b) -> IO b
    default put :: Storable a => a -> (MDB_val -> IO b) -> IO b
    put x f = allocaBytes (sizeOf x) (\p -> f (MDB_val (fromIntegral (sizeOf x)) p))

-- more instances

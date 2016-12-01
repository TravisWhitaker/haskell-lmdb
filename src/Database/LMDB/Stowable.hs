{-# LANGUAGE DefaultSignatures #-}

module Database.LMDB.Stowable (
    Stowable(..)
  ) where

import Control.DeepSeq

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Database.LMDB.Internal.Raw

import Data.ByteString   as B
import Data.Text
import Data.Text.Foreign as T

class NFData a => Stowable a where
    get :: MDB_val -> IO a
    default get :: Storable a => MDB_val -> IO a
    get (MDB_val _ p) = force <$> peek (castPtr p)
    put :: a -> (MDB_val -> IO b) -> IO b
    default put :: Storable a => a -> (MDB_val -> IO b) -> IO b
    put x f = allocaBytes (sizeOf x) (\p -> f (MDB_val (fromIntegral (sizeOf x)) p))

-- more instances

instance Stowable ByteString where
    get (MDB_val size ptr) = B.packCStringLen (castPtr ptr, fromIntegral size)
    put bstr cont = bstr `B.useAsCStringLen` \(ptr, size) ->
        cont $ fromIntegral size `MDB_val` castPtr ptr

instance Stowable Text where
    get (MDB_val size ptr) = T.peekCStringLen (castPtr ptr, fromIntegral size)
    put bstr cont = bstr `T.withCStringLen` \(ptr, size) ->
        cont $ fromIntegral size `MDB_val` castPtr ptr

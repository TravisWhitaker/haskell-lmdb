{-# LANGUAGE DefaultSignatures #-}

module Database.LMDB.Stowable (
    Stowable(..)
  , encode
  , decode
  ) where

import Control.DeepSeq
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Database.LMDB.Internal.Raw

import Data.ByteString   as B
import Data.Text
import Data.Text.Foreign as T

import Debug.Trace

class NFData a => Stowable a where
    get :: MDB_val -> IO a
    default get :: Storable a => MDB_val -> IO a
    get (MDB_val _ p) = force <$> peek (castPtr p)
    put :: a -> (MDB_val -> IO b) -> IO b
    default put :: Storable a => a -> (MDB_val -> IO b) -> IO b
    put x f = allocaBytes (sizeOf x) (\p -> f (MDB_val (fromIntegral (sizeOf x)) p))

-- more instances

instance Stowable ByteString where
    get (MDB_val size ptr) = do
        str <- B.packCStringLen (castPtr ptr, fromIntegral size)
        print ("decode", size, str)
        return str
        
    put bstr cont = bstr `B.useAsCStringLen` \(ptr, size) -> do
        print ("encode", size, bstr)
        cont $ fromIntegral size `MDB_val` castPtr ptr

instance Stowable Text where
    get (MDB_val size ptr) = T.peekCStringLen (castPtr ptr, fromIntegral size)
    put bstr cont = bstr `T.withCStringLen` \(ptr, size) ->
        cont $ fromIntegral size `MDB_val` castPtr ptr

encode thing = liftIO $ put thing return
decode thing = liftIO $ get thing

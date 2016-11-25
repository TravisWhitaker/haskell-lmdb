
{-# language TypeFamilies     #-}
{-# language LambdaCase       #-}
{-# language FlexibleContexts #-}

module Database.LMDB.Interpreter.LMDB where

import Control.Monad.Free.Church
import Control.Monad.Reader

import Database.LMDB.Internal.Raw
import Database.LMDB.Stowable     as S
import Database.LMDB.TxM

import Foreign.Storable
import Foreign.Marshal.Alloc

data LMDB

newtype DBW     payload k v = DBW     payload
newtype CursorW payload k v = CursorW payload
newtype MoveW   payload k   = MoveW   payload

instance TxInterp LMDB where
    type TxDB     LMDB = DBW     MDB_dbi
    type TxCursor LMDB = CursorW MDB_cursor
    type TxMove   LMDB = MoveW   MDB_cursor_op
    type TxMonad  LMDB = ReaderT (MDB_txn, [MDB_WriteFlag])

    interpTxT tree = runF tree return algebra
      where
        algebra layer = do
            (trans, flags) <- ask
            case layer of
              Lift action continue -> do
                lift action >>= continue

              Drop (DBW db) next -> do
                liftIO $ mdb_drop trans db
                next

              Clear (DBW db) next -> do
                liftIO $ mdb_clear trans db
                next

              Get (DBW db) key consume -> do
                key'    <- encode key
                result  <- liftIO $ mdb_get trans db key'
                decoded <- traverse decode result
                consume decoded

              Put (DBW db) key value continue -> do
                key'    <- encode key
                value'  <- encode value
                success <- liftIO $ mdb_put (c flags) trans db key' value'
                continue success

              Upsert (DBW db) key value continue -> do
                key'    <- encode key
                value'  <- encode value
                let flags' = MDB_APPENDDUP : flags
                success <- liftIO $ mdb_put (c flags') trans db key' value'
                continue success

              Del (DBW db) key continue -> do
                key'    <- encode key
                success <- liftIO $ mdb_del trans db key' Nothing
                continue success

              OpenCursor (DBW db) consume -> do
                cursor <- liftIO $ mdb_cursor_open trans db
                consume (CursorW cursor)

              MoveCursor (CursorW cursor) (MoveW move) consumePair -> do
                maybePair <- liftIO $
                    alloca $ \keyPtr ->
                    alloca $ \valPtr -> do
                        success <- mdb_cursor_get move cursor keyPtr valPtr
                        if success
                        then do
                            key  <- peek keyPtr
                            val  <- peek valPtr
                            key' <- decode key
                            val' <- decode val
                            return $ Just (key' `Pair` val')
                        else do
                            return   Nothing
                consumePair maybePair

              PutCursor (CursorW cursor) key value continue -> do
                key' <- encode key
                val' <- encode value
                success <- liftIO $ mdb_cursor_put (c flags) cursor key' val'
                continue success

              DelCursor (CursorW cursor) _key next -> do
                liftIO $ mdb_cursor_del (c flags) cursor
                next

              CloseCursor (CursorW cursor) next -> do
                liftIO $ mdb_cursor_close cursor
                next

              Abort -> do
                error "is there any meaning here?"

        c = compileWriteFlags
        encode thing = liftIO $ S.put thing return
        decode thing = liftIO $ S.get thing

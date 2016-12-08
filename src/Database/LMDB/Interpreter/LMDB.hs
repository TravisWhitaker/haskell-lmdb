
{-# language TypeFamilies     #-}
{-# language LambdaCase       #-}
{-# language FlexibleContexts #-}

module Database.LMDB.Interpreter.LMDB where

import Control.Monad.Free.Church
import Control.Monad.Reader

import Database.LMDB.Internal.Raw
import Database.LMDB.Types
import Database.LMDB.Stowable     as S
import Database.LMDB.TxM

import Foreign.Storable
import Foreign.Marshal.Alloc

-- * Type label for main lmbd-DSL interpreter.
data LMDB

-- * Wrappers for cursor and cursor movement.
newtype CursorW payload k v = CursorW payload
newtype MoveW   payload k   = MoveW   payload

-- * Main way to interpret DSL in "battle" situation.
--   FIXME(kir): should it be a method of TxInterp, too?
withTransaction
    :: (Env mode, [MDB_WriteFlag])
    -> TxT LMDB IO a
    -> IO a
withTransaction (Env env, flags) action = do
    txn <- mdb_txn_begin env Nothing False
    res <- interpTxT action `runReaderT` (txn, compileWriteFlags flags)
    mdb_txn_commit txn
    return res

instance TxInterp LMDB where
    type TxDB         LMDB = DB
    type TxCursor     LMDB = CursorW MDB_cursor'
    type TxMonad      LMDB = ReaderT (MDB_txn, MDB_WriteFlags)
    type TxConstraint LMDB = MonadIO

    interpTxT tree = runF tree return algebra
      where
        algebra layer = do
            (trans, flags) <- ask
            case layer of
              Lift action continue -> do
                lift action >>= continue

              Drop (DB db) next -> do
                liftIO $ mdb_drop' trans db
                next

              Clear (DB db) next -> do
                liftIO $ mdb_clear' trans db
                next

              Get (DB db) key consume -> do
                key'    <- encode key
                result  <- liftIO $ mdb_get' trans db key'
                decoded <- traverse decode result
                consume decoded

              Put (DB db) key value continue -> do
                key'    <- encode key
                value'  <- encode value
                success <- liftIO $ mdb_put' flags trans db key' value'
                continue success

              Upsert (DB db) key value continue -> do
                key'    <- encode key
                value'  <- encode value
                -- FIXME(kir): I'm not sure about that flag.
                -- TODO: write tests
                let flags' = flags `setFlag` MDB_APPENDDUP
                success <- liftIO $ mdb_put' flags' trans db key' value'
                continue success

              Del (DB db) key continue -> do
                key'    <- encode key
                success <- liftIO $ mdb_del' trans db key' Nothing
                continue success

              -- TODO: Understand cursors and write tests.
              OpenCursor (DB db) consume -> do
                cursor <- liftIO $ mdb_cursor_open' trans db
                consume (CursorW cursor)

              MoveCursor (CursorW cursor) move consumePair -> do
                maybePair <- liftIO $
                    alloca $ \keyPtr ->
                    alloca $ \valPtr -> do
                        move' <- case move of
                          MoveTo k -> do
                            key' <- encode k
                            keyPtr `poke` key'
                            return MDB_FIRST
                          MoveNext ->
                            return MDB_NEXT
                          MoveGetItem ->
                            return MDB_GET_CURRENT
                        
                        success <- mdb_cursor_get' move' cursor keyPtr valPtr
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
                success <- liftIO $ mdb_cursor_put' flags cursor key' val'
                continue success

              DelCursor (CursorW cursor) _key next -> do
                liftIO $ mdb_cursor_del' flags cursor
                next

              CloseCursor (CursorW cursor) next -> do
                liftIO $ mdb_cursor_close' cursor
                next

              Abort -> do
                error "is there any meaning here?"

        encode thing = liftIO $ S.put thing return
        decode thing = liftIO $ S.get thing


{-# language TypeFamilies     #-}
{-# language LambdaCase       #-}
{-# language FlexibleContexts #-}

module Database.LMDB.Interpreter.LMDB where

import Control.Monad.Catch
import Control.Monad.Free.Church
import Control.Monad.Reader

import Database.LMDB.Internal.Raw
import Database.LMDB.Types
import Database.LMDB.Stowable     as S
import Database.LMDB.TxM

import Foreign.Storable
import Foreign.Marshal.Alloc

import System.FilePath

data LMDB

newtype DBW     payload k v = DBW     payload
newtype CursorW payload k v = CursorW payload
newtype MoveW   payload k   = MoveW   payload

-- openEnvironment
--     :: MonadMask m
--     => MonadIO m
--     => String
--     -> [MDB_WriteFlag]
--     -> [MDB_EnvFlag]
--     -> ReaderT (MDB_env, [MDB_WriteFlag]) m a
--     -> m a
-- openEnvironment name flags dbFlags action = do
--     bracket open close (\env -> runReaderT action (env, flags))
--   where
--     close = liftIO . mdb_env_close
--     open = liftIO $ do
--         env <- mdb_env_create
--         env `mdb_env_set_mapsize`   (1024 * 1024 * 1024)
--         env `mdb_env_set_maxreaders` 20000
--         env `mdb_env_set_maxdbs`     64
--         mdb_env_open env name dbFlags
--         return env

withTransaction
    :: (Env mode, [MDB_WriteFlag])
    -> TxT LMDB IO a
    -> IO a
withTransaction (Env env, flags) action = do
    txn <- mdb_txn_begin env Nothing False
    res <- interpTxT action `runReaderT` (txn, flags)
    mdb_txn_commit txn
    return res

instance TxInterp LMDB where
    type TxDB         LMDB = DB
    type TxCursor     LMDB = CursorW MDB_cursor'
    type TxMove       LMDB = MoveW   MDB_cursor_op
    type TxMonad      LMDB = ReaderT (MDB_txn, [MDB_WriteFlag])
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
                success <- liftIO $ mdb_put' (c flags) trans db key' value'
                continue success

              Upsert (DB db) key value continue -> do
                key'    <- encode key
                value'  <- encode value
                let flags' = MDB_APPENDDUP : flags
                success <- liftIO $ mdb_put' (c flags') trans db key' value'
                continue success

              Del (DB db) key continue -> do
                key'    <- encode key
                success <- liftIO $ mdb_del' trans db key' Nothing
                continue success

              OpenCursor (DB db) consume -> do
                cursor <- liftIO $ mdb_cursor_open' trans db
                consume (CursorW cursor)

              MoveCursor (CursorW cursor) (MoveW move) consumePair -> do
                maybePair <- liftIO $
                    alloca $ \keyPtr ->
                    alloca $ \valPtr -> do
                        success <- mdb_cursor_get' move cursor keyPtr valPtr
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
                success <- liftIO $ mdb_cursor_put' (c flags) cursor key' val'
                continue success

              DelCursor (CursorW cursor) _key next -> do
                liftIO $ mdb_cursor_del' (c flags) cursor
                next

              CloseCursor (CursorW cursor) next -> do
                liftIO $ mdb_cursor_close' cursor
                next

              Abort -> do
                error "is there any meaning here?"

        c = compileWriteFlags
        encode thing = liftIO $ S.put thing return
        decode thing = liftIO $ S.get thing

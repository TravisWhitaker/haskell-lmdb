{-# LANGUAGE BangPatterns
           , GADTs
           , TypeFamilies
           , ConstraintKinds
           #-}

module Database.LMDB.TxM (
    Pair(..)
  , TxInterp(..)
  , TxOp(..)
  , TxMove(..)
  , TxT
  , liftTxT
  , drop
  , clear
  , get
  , gets
  , put
  , upsert
  , del
  , openCursor
  , moveCursor
  , putCursor
  , delCursor
  , closeCursor
  , abort
  ) where

import Control.Monad.Free.Church

import Database.LMDB.Stowable (Stowable)

import GHC.Exts (Constraint)

import Prelude hiding (drop)

import Debug.Trace

data Pair k v = Pair !k !v
    deriving (Show)

class TxInterp i where
    type TxDB i     :: * -> * -> *
    type TxCursor i :: * -> * -> *
    type TxMonad i  :: (* -> *) -> * -> *
    type TxConstraint i :: (* -> *) -> Constraint
    interpTxT :: TxConstraint i m => TxT i m a -> (TxMonad i) m a

data TxOp i m x where
    Lift        :: Monad m                              => m a -> (a -> x) -> TxOp i m x
    Drop        :: TxInterp i                           => (TxDB i) k v -> x -> TxOp i m x
    Clear       :: TxInterp i                           => (TxDB i) k v -> x -> TxOp i m x
    Get         :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> ((Maybe v) -> x) -> TxOp i m x
    Put         :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> v -> (Bool -> x) -> TxOp i m x
    -- Upsert      :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> v -> (Bool -> x) -> TxOp i m x
    Del         :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> (Bool -> x) -> TxOp i m x
    OpenCursor  :: (TxInterp i)                         => (TxDB i) k v -> (((TxCursor i) k v) -> x) -> TxOp i m x
    MoveCursor  :: (TxInterp i, Stowable k, Stowable v) => (TxCursor i) k v -> (TxMove i) k -> ((Maybe (Pair k v)) -> x) -> TxOp i m x
    PutCursor   :: (TxInterp i, Stowable k, Stowable v) => (TxCursor i) k v -> k -> v -> (Bool -> x) -> TxOp i m x
    DelCursor   :: (TxInterp i, Stowable k)             => (TxCursor i) k v -> k -> x -> TxOp i m x
    CloseCursor :: (TxInterp i)                         => (TxCursor i) k v -> x -> TxOp i m x
    Abort       :: TxOp i m x

data TxMove i k
    = MoveTo k
    | MoveNext

instance Functor (TxOp i m) where
    fmap f (Lift m x)          = Lift m (f . x)
    fmap f (Drop d x)          = Drop d (f x)
    fmap f (Clear d x)         = Clear d (f x)
    fmap f (Get d k x)         = Get d k (f . x)
    fmap f (Put d k v x)       = Put d k v (f . x)
    -- fmap f (Upsert d k v x)    = Upsert d k v (f . x)
    fmap f (Del d k x)         = Del d k (f . x)
    fmap f (OpenCursor d x)    = OpenCursor d (f . x)
    fmap f (MoveCursor c m x)  = MoveCursor c m (f . x)
    fmap f (PutCursor c k v x) = PutCursor c k v (f . x)
    fmap f (DelCursor c k x)   = DelCursor c k (f x)
    fmap f (CloseCursor c x)   = CloseCursor c (f x)
    fmap _ Abort               = Abort

type TxT i m a = F (TxOp i m) a

liftTxT :: Monad m => m a -> TxT i m a
liftTxT m = liftF $ Lift m id

drop :: TxInterp i => (TxDB i) k v -> TxT i m ()
drop db = liftF $ Drop db ()

clear :: TxInterp i => (TxDB i) k v -> TxT i m ()
clear db = liftF $ Clear db ()

get :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> TxT i m (Maybe v)
get db k = liftF $ Get db k id

gets
    :: (TxInterp i, Stowable k, Stowable v, Show k, Show v)
    => TxDB i k v -> k -> TxT i m [v]
gets db k = do
    cur <- openCursor db
    mp  <- moveCursor cur (MoveTo k)
    case mp of
      Nothing ->
        return []
      Just (Pair k v) -> do
        vs <- retrieveAllAt cur
        return (v : vs)
  where
    retrieveAllAt cur = do
        maybePair <- moveCursor cur MoveNext
        case maybePair of
          Nothing -> do
            return []
          Just (Pair _ v) -> do
            vs <- retrieveAllAt cur
            return (v : vs)

put :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> v -> TxT i m Bool
put db k v = liftF $ Put db k v id

upsert :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> v -> TxT i m Bool
upsert db k v = do
    cur <- openCursor db
    _   <- moveCursor cur (MoveTo k)
    res <- putCursor cur k v
    return res

del :: (TxInterp i, Stowable k, Stowable v) => (TxDB i) k v -> k -> TxT i m Bool
del db k = liftF $ Del db k id

openCursor :: TxInterp i => (TxDB i) k v -> TxT i m ((TxCursor i) k v)
openCursor db = liftF $ OpenCursor db id

moveCursor :: (TxInterp i, Stowable k, Stowable v) => (TxCursor i) k v -> (TxMove i) k -> TxT i m (Maybe (Pair k v))
moveCursor c m = liftF $ MoveCursor c m id

putCursor :: (TxInterp i, Stowable k, Stowable v) => (TxCursor i) k v -> k -> v -> TxT i m Bool
putCursor c k v = liftF $ PutCursor c k v id

delCursor :: (TxInterp i, Stowable k) => (TxCursor i) k v -> k -> TxT i m ()
delCursor c k = liftF $ DelCursor c k ()

closeCursor :: TxInterp i => (TxCursor i) k v -> TxT i m ()
closeCursor c = liftF $ CloseCursor c ()

abort :: TxT i m a
abort = liftF Abort


{-# language OverloadedStrings #-}

import Control.Monad.Reader

import Control.Concurrent.Thread (result, forkOS)

import Database.LMDB.Interpreter.LMDB
import Database.LMDB.TxM
import Database.LMDB

import Data.ByteString

import Test.QuickCheck
import Test.QuickCheck.Monadic

bs :: ByteString -> ByteString
bs = id

main :: IO ()
main = do
    let envCfg = def { noLock = True }
    withEnv envCfg "database" (1024 * 1024) $ ReaderT $ \env ->
        withDB env (Named "other") def      $ ReaderT $ \dbi -> do
            quickCheck $ \key value ->
                let
                  command = do
                    put    dbi (bs key) (bs value)
                    get    dbi (bs key)

                in test command (Just value) env

test
    :: Eq a
    => TxT LMDB IO a
    -> a
    -> Env mode
    -> Property
test command result env' =
    monadicIO $ do
        res <- run $ withTransaction (env', []) command
        assert False -- (res /= result)

instance Arbitrary ByteString where
    arbitrary = pack <$> arbitrary

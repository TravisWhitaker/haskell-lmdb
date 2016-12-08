
{-# language OverloadedStrings #-}

import Control.Monad.Reader

import Control.Concurrent.Thread (result, forkOS)

import Database.LMDB.Interpreter.LMDB
import Database.LMDB.TxM
import Database.LMDB

import qualified Data.ByteString
import Data.ByteString (ByteString, pack)
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Monadic

bs :: ByteString -> ByteString
bs = ("bs" <>)

main :: IO ()
main = do
    (_, res) <- forkOS $
        withEnv def "database" (1024 * 1024) $ ReaderT $ \env ->
        withDB  def (Named "other") env      $ ReaderT $ \dbi -> do
            -- * Sanity check: read after write.
            quickCheck $ \key value ->
                let
                  k = pack key
                  v = pack value
                  command = do
                    _ <- put dbi (bs k) (bs v)
                    get dbi (bs k)

                in test command (Just (bs v)) env

            -- * Sanity check is slow, but this is not.
            quickCheck $ \key ->
                Data.ByteString.length (pack key) >= 0
                
            quickCheck $ \key vals ->
                let
                  k  =     pack key
                  vs = map pack vals
                  command = do
                    forM_ (take 10 vs) $ \v ->
                        put dbi (bs k) (bs v)
                        
                    gets dbi k
                    
                in test command (map bs vs)  env
    res' <- res
    result res'

-- * Run given DSL expression in a transaction.
test
    :: Eq a
    => TxT LMDB IO a
    -> a
    -> Env mode
    -> Property
test command expected env' =
    monadicIO $ do
        res <- run $ withTransaction (env', []) command
        assert (res == expected)

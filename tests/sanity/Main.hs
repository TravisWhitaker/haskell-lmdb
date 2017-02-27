
{-# language OverloadedStrings #-}

import Control.Monad.Reader

import Control.Concurrent.Thread (result, forkOS)

import Database.LMDB.Interpreter.LMDB
import qualified Database.LMDB.Stowable as Store (encode, decode)
import Database.LMDB.TxM
import Database.LMDB

import qualified Data.ByteString
import Data.ByteString.Char8 (ByteString, pack)
import Data.Monoid
import Data.List (sort, nub)

import GHC.Exts

import Prelude hiding (drop)

import Test.QuickCheck
import Test.QuickCheck.Monadic

bs :: ByteString -> ByteString
bs = ("bs" <>)

main :: IO ()
main = do
    (_, res) <- forkOS $
        withEnv def "database" (1024 * 1024) $ ReaderT $ \env ->
        withDB  def { dupSort = True, dupFixed = True } (Named "other") env      $ ReaderT $ \dbi -> do
            -- quickCheck $ \bytes ->
            --     monadicIO $ do
            --         let victim = (pack bytes)
            --         enc <- run $ Store.encode victim
            --         dec <- run $ Store.decode enc
            --         
            --         return (dec == victim)
            
            quickCheck $
                let
                  fs = fromString
                  write = upsert
                  command = do
                    clear dbi
                    write dbi (fs "a") (fs "q")
                    write dbi (fs "a") (fs "uiop")
                    write dbi (fs "a") (fs "we")
                    write dbi (fs "a") (fs "rty")
                    write dbi (fs "a") (fs "asdfg")
                    gets dbi (fs "a")
                in
                    test id command
                        (map fs ["q", "we", "rty", "uiop", "asdfg"])
                        env
        
            quickCheck $ \key vals ->
                let
                  command = do
                    clear dbi
                    forM_ vals $ \val ->
                        put dbi key (val :: ByteString)
                        
                    gets dbi (key :: ByteString)
                    
                in test (nub . sort) command vals env
            -- * Sanity check: read after write.
            -- quickCheck $ \key value ->
            --     let
            --       k = pack key
            --       v = pack value
            --       command = do
            --         clear dbi
            --         _ <- put dbi (bs k) (bs v)
            --         get dbi (bs k)
            -- 
            --     in test id command (Just (bs v)) env
            -- 
            -- -- * Sanity check is slow, but this is not.
            -- quickCheck $ \key ->
            --     Data.ByteString.length (pack key) >= 0
                
    res' <- res
    result res'

-- * Run given DSL expression in a transaction.
test
    :: Eq b
    => Show b
    => Show a
    => (a -> b)
    -> TxT LMDB IO a
    -> a
    -> Env mode
    -> Property
test project command expected env' =
    monadicIO $ do
        res <- run $ withTransaction (env', []) command
        run $ print ("res", project res)
        run $ print ("exp", project expected)
        run $ print ("==")
        assert (project res == project expected)
        
newtype Letters = Letters { getLetters :: [Char] }

instance Arbitrary Letters where
    arbitrary = Letters <$> do
        listOf1 $ suchThat arbitrary $ flip elem $ ['a'.. 'z'] ++ "-"
        
instance Arbitrary ByteString where
    arbitrary = (fromString . getLetters) <$> arbitrary

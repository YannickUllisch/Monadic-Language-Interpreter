module KVDB_Tests (tests) where

import KVDB
import Test.Tasty (TestTree, testGroup)
import Control.Concurrent (threadDelay, forkIO)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "KVDB"
    [ testCase "Basic Put & Get Test" $ do
        kvdb <- startKVDB
        kvPut kvdb "key" (2 :: Int)
        v <- kvGet kvdb "key"
        v @?= 2,
      testCase "Test Finding Correct Value" $ do
        kvdb <- startKVDB
        kvPut kvdb "key1" (2 :: Int)
        kvPut kvdb "key2" (5 :: Int)
        kvPut kvdb "key3" (10 :: Int)
        v <- kvGet kvdb "key3"
        v @?= 10,
      testCase "Testing Polymorphic Val String" $ do
        kvdb <- startKVDB
        kvPut kvdb "key1" ("Test" :: String)
        v <- kvGet kvdb "key1"
        v @?= "Test",
      testCase "Testing Polymorphic Val Bool" $ do
        kvdb <- startKVDB
        kvPut kvdb "key1" $ True
        v <- kvGet kvdb "key1"
        v @?= True,
      testCase "Blocking for Value" $ do
        kvdb <- startKVDB  
        -- Create new thread to put value into db after some delay
        _ <- forkIO $ do
          threadDelay 2000000
          kvPut kvdb "key1" True
        -- Retrieve the value in the main test thread - should block until kvPut is executed
        val <- kvGet kvdb "key1"
        val @?= True,
      testCase "Overwriting Value" $ do
        kvdb <- startKVDB
        kvPut kvdb "key" (2 :: Int)
        kvPut kvdb "key" (5 :: Int)
        v <- kvGet kvdb "key"
        v @?= 5,
      testCase "Multiple Blocking Gets" $ do
        kvdb <- startKVDB
        _ <- forkIO $ do
          v1 <- kvGet kvdb "multiBlock"
          v1 @?= 5

        _ <- forkIO $ do
          v2 <- kvGet kvdb "multiBlock"
          v2 @?= 5
          
        threadDelay 2000000
        kvPut kvdb "multiBlock" (5 :: Int),
      testCase "Blocking Get for Missing Key" $ do
        kvdb <- startKVDB

        _ <- forkIO $ do
          v <- kvGet kvdb "blocking"
          v @?= 10

        threadDelay 2000000
        kvPut kvdb "blocking" (10 :: Int)
    ]

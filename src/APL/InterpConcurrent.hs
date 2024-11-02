{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module APL.InterpConcurrent (runEval) where

import APL.Monad
--import Data.IORef
import KVDB
import SPC
import Control.Concurrent.MVar

runEval :: EvalM a -> IO (Either Error a)
runEval m = do
    kvdb <- startKVDB
    runEval' envEmpty kvdb m
  where
    runEval' :: Env -> KVDB Val Val -> EvalM a -> IO (Either Error a)
    runEval' _ _ (Pure x) = pure $ pure x
    runEval' e db (Free (ReadOp f)) = runEval' e db $ f e
    runEval' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEval' r db (Free (KvGetOp key k)) = do
      res <- kvGet db key
      runEval' r db $ k res
    runEval' r db (Free (KvPutOp key val cont)) = do
      kvPut db key val
      runEval' r db cont
    runEval' r db (Free (StepOp cont)) = runEval' r db cont
    runEval' r db (Free (BothOfOp e1 e2 c)) = do
        spc <- startSPC
        result1 <- newEmptyMVar
        result2 <- newEmptyMVar

        _ <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEval' r db e1
                putMVar result1 res1
        }

        _ <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEval' r db e2
                putMVar result2 res2
        }

        res1 <- takeMVar result1
        res2 <- takeMVar result2
        case (res1, res2) of
            (Right v1, Right v2) -> runEval' r db (c (ValTuple [v1, v2]))
            (Left e, _) -> pure (Left e)
            (_, Left e) -> pure (Left e)
    runEval' r db (Free (OneOfOp e1 e2 c)) = do
        spc <- startSPC
        initRes <- newEmptyMVar

        jid1 <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEval' r db e1
                putMVar initRes res1
        }

        jid2 <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEval' r db e2
                putMVar initRes res2
        }

        res <- takeMVar initRes

        -- We cancel both jobs to avoid consuming resources
        jobCancel spc jid1
        jobCancel spc jid2

        case res of
            Right val -> runEval' r db $ c val
            Left e -> pure $ Left e
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC

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

        ref1 <- newIORef (Left "Job not completed")
        ref2 <- newIORef (Left "Job not completed")

        jid1 <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEval' r db e1
                writeIORef ref1 res1
        }

        jid2 <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEval' r db e2
                writeIORef ref2 res2
        }

        -- Wait for both jobs to be completed 
        (_, reason1) <- jobWaitAny spc [jid1]
        (_, reason2) <- jobWaitAny spc [jid2]

        -- Matching reasons, we only return the tuple if both Jobs completed successfully
        case (reason1, reason2) of
            (Done, Done) -> do
                res1 <- readIORef ref1
                res2 <- readIORef ref2
                case (res1, res2) of
                    (Right v1, Right v2) -> runEval' r db (c (ValTuple [v1, v2]))
                    (Left e, _) -> pure (Left e)
                    (_, Left e) -> pure (Left e)
            (Done, _) -> pure $ Left "Job 2 did failed"
            (_, Done) -> pure $ Left "Job 1 did failed"
            _ -> pure $ Left "Both jobs failed"
    runEval' r db (Free (OneOfOp e1 e2 c)) = do
        spc <- startSPC

        ref1 <- newIORef (Left "Job not completed")
        ref2 <- newIORef (Left "Job not completed")


        jid2 <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEval' r db e2
                writeIORef ref2 res2
        }
        jid1 <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEval' r db e1
                writeIORef ref1 res1
        }

        -- Wait for both jobs to be completed 
        (fstId, rsn1) <- jobWaitAny spc [jid1, jid2]

        -- Helper function to find correct return value
        let readResult jid = readIORef $ if jid == jid1 then ref1 else ref2

        
        -- Helper function that reads the result from a done job
        -- and matches its output
        let processDoneJob jid = do
                res <- readResult jid
                case res of
                    Right val -> runEval' r db $ c val
                    Left e -> pure $ Left e

        -- Mapping first completed job and remaining job to a tuple
        -- we need these to correctly check job results for failures
        let (firstCompleted, remainingJid) = (fstId, if fstId == jid1 then jid2 else jid1)

        case rsn1 of
            Done -> do
                res1 <- readResult firstCompleted
                case res1 of
                    Right val -> do
                        jobCancel spc remainingJid -- Cancelling remaining job
                        runEval' r db $ c val
                    Left _ -> do  
                        -- We first check status, since it might have completed while executing the above code
                        status <- jobStatus spc remainingJid
                        case status of
                            JobDone Done -> processDoneJob remainingJid
                            _ -> do
                                -- If not completed we wait for it to complete
                                (_, rsn2) <- jobWaitAny spc [remainingJid]
                                case rsn2 of
                                    Done -> processDoneJob remainingJid
                                    _ -> pure $ Left "Both Jobs failed"
            _ -> pure $ Left "First job did not complete successfully"
            
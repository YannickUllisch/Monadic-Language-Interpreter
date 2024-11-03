{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module APL.InterpConcurrent (runEval) where

import APL.Monad
import Data.IORef
import KVDB
import SPC

runEval :: EvalM a -> IO (Either Error a)
runEval m = do
    kvdb <- startKVDB
    spc <- startSPC
    runEvalCC' envEmpty spc kvdb m
  where
    runEvalCC' :: Env -> SPC -> KVDB Val Val -> EvalM a -> IO (Either Error a)
    runEvalCC' _ _ _ (Pure x) = pure $ pure x
    runEvalCC' e spc db (Free (ReadOp f)) = runEvalCC' e spc db $ f e
    runEvalCC' _ _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalCC' r spc db (Free (KvGetOp key k)) = do
      res <- kvGet db key
      runEvalCC' r spc db $ k res
    runEvalCC' r spc db (Free (KvPutOp key val cont)) = do
      kvPut db key val
      runEvalCC' r spc db cont
    runEvalCC' r spc db (Free (StepOp cont)) = runEvalCC' r spc db cont
    runEvalCC' r spc db (Free (BothOfOp e1 e2 c)) = do
        ref1 <- newIORef (Left "Job not completed")
        ref2 <- newIORef (Left "Job not completed")

        -- Running Jobs
        jid2 <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEvalCC' r spc db e2
                writeIORef ref2 res2
        }
        jid1 <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEvalCC' r spc db e1
                writeIORef ref1 res1
        }

        -- Wait for both jobs to be completed 
        (_, reason1) <- jobWaitAny spc [jid1]
        (_, reason2) <- jobWaitAny spc [jid2]

        -- Checking results of both jobs and processing them accordingly
        case (reason1, reason2) of
            (Done, Done) -> do
                res1 <- readIORef ref1
                res2 <- readIORef ref2
                case (res1, res2) of
                    (Right x, Right y) -> runEvalCC' r spc db $ c $ ValTuple [x, y]
                    (Left e, _) -> pure (Left e)
                    (_, Left e) -> pure (Left e)
            (Done, _) -> pure $ Left "Second Job failed"
            (_, Done) -> pure $ Left "Job 1 failed"
            _ -> pure $ Left "Both jobs failed"
    runEvalCC' r spc db (Free (OneOfOp e1 e2 c)) = do
        -- Separate refs to avoid any write race conditions
        ref1 <- newIORef (Left "Job not completed")
        ref2 <- newIORef (Left "Job not completed")

        -- Running Jobs
        jid2 <- jobAdd spc $ Job {
            jobAction = do
                res2 <- runEvalCC' r spc db e2
                writeIORef ref2 res2
        }
        jid1 <- jobAdd spc $ Job {
            jobAction = do
                res1 <- runEvalCC' r spc db e1
                writeIORef ref1 res1
        }

        -- Wait for one of the jobs to be completed
        (tmpId, reason) <- jobWaitAny spc [jid1, jid2]

        -- Mapping first completed job and remaining job to a tuple
        -- we need these to correctly check job results for failures
        let (fstId, sndId) = (tmpId, if tmpId == jid1 then jid2 else jid1)

        -- Helper function to find correct return value
        let readResult jid = readIORef $ if jid == jid1 then ref1 else ref2
        
        -- Helper function that reads the result from a done job
        -- and matches its output
        let processDoneJob jid = do
                res <- readResult jid
                case res of
                    Right val -> runEvalCC' r spc db $ c val
                    Left e -> pure $ Left e

        -- Helper function to check remaining job in case of first completed not being valid
        let checkRemaining jid = do
                status <- jobStatus spc jid
                case status of
                            JobDone Done -> processDoneJob jid
                            JobsRunning -> do
                                -- If not completed we wait for it to complete
                                (_, doneReason) <- jobWaitAny spc [jid]
                                case doneReason of
                                    Done -> processDoneJob jid
                                    _ -> pure $ Left "Both jobs failed"
                            _ -> pure $ Left "Error in JobId processing"
                            

        -- Checking results of Job and processing accordingly
        case reason of
            Done -> do
                res1 <- readResult fstId
                case res1 of
                    Right val -> do
                        jobCancel spc sndId -- Cancelling remaining job
                        runEvalCC' r spc db $ c val
                    Left _ -> checkRemaining sndId
            _ -> checkRemaining sndId
            
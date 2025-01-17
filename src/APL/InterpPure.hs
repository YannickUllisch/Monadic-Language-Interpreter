module APL.InterpPure (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

runEval :: EvalM a -> Either Error a
runEval = fst <$> runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> (Either Error a, State)
    runEval' _ s (Pure x) = (pure x, s)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Nothing -> (Left $ "Invalid key: " ++ show key, s)
        Just val -> runEval' r s $ k val
    runEval' r s (Free (KvPutOp key val m)) =
      let s' = (key, val) : filter ((/= key) . fst) s
        in runEval' r s' m
    runEval' _ s (Free (ErrorOp e)) = (Left e, s)
    runEval' r s (Free (StepOp c)) = runEval' r s c
    runEval' r s (Free (BothOfOp e1 e2 c)) = do
      let (res1, s1) = runEval' r s e1
      let (res2, s2) = runEval' r s1 e2
      
      case (res1, res2) of
            (Right x, Right y) -> runEval' r s2 $ c $ ValTuple [x, y]
            (Left e, _) -> (Left e, s1) -- Since we assume left to right execution, if first fails state updates from second should be ommitted
            (_, Left e) -> (Left e, s2)
    runEval' r s (Free (OneOfOp e1 e2 c)) =
      case runEval' r s e1 of
        (Right res, s1) -> runEval' r s1 $ c res
        (Left _, s1) -> case runEval' r s1 e2 of
          (Right res2, s2) -> runEval' r s2 $ c res2
          (Left e, s2) -> (Left e, s2)

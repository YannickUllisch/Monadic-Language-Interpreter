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
    runEval' r s (Free (StepOp contin)) = runEval' r s contin
    runEval' r s (Free (BothOfOp e1 e2 c)) =
      let (res1, s') = runEval' r s e1
          (res2, s'') = runEval' r s' e2
      in case (res1, res2) of
            (Right vx, Right vy) -> runEval' r s'' $ c (ValTuple [vx, vy])
            (Left e, _) -> (Left e, s'')
            (_, Left e) -> (Left e, s'')
    runEval' r s (Free (OneOfOp e1 e2 c)) =
      case runEval' r s e1 of
        (Right res, s') -> runEval' r s' $ c res
        (Left _, s') -> case runEval' r s' e2 of
          (Right res2, s'') -> runEval' r s'' $ c res2
          (Left e, s'') -> (Left e, s'')

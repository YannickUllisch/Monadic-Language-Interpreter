module APL.InterpSim (runEval) where

import APL.Monad

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

-- | Continue execution of the provided computation as far as
-- possible, but executing at most one 'StepOp' effect. Any nested
-- computations (in 'BothOp' and 'OneOfOp') must also be stepped
-- similarly. If the computation is stuck on a 'KvGetOp' for which the
-- key is not in the state, then the computation is merely returned
-- unchanged.
step :: Env -> State -> EvalM a -> (EvalM a, State)
step _ s (Pure x) = (Pure x, s)
step _ s (Free (ErrorOp err)) = (Free (ErrorOp err), s)
step e s (Free (ReadOp k)) = (k e, s)
step _ s (Free (KvGetOp key next)) =
    case lookup key s of
        Just val -> (next val, s)
        Nothing  -> (Free (KvGetOp key next), s)
step _ s (Free (KvPutOp key val next)) =
    (next, (key, val) : s)
step _ s (Free (StepOp next)) = (next, s)
step e s (Free (BothOfOp e1 e2 next)) = do
    -- Executing from left to right, ensuring state is correctly
    -- updated in between 
    let (res1, s1) = step e s e1
    let (res2, s2) = step e s1 e2

    case (res1, res2) of
        (Free (ErrorOp err), _) -> (Free (ErrorOp err), s1)
        (_, Free (ErrorOp err)) -> (Free (ErrorOp err), s2)
        (Pure x, Pure y) -> (next (ValTuple [x, y]), s2)
        _ -> (Free (BothOfOp res1 res2 next), s2)
step e s (Free (OneOfOp e1 e2 next)) = do
    -- Execute the first step of e1
    let (res1, s1) = step e s e1

    -- Implementation of logic that allows us
    -- to stop potential infinite loops
    case res1 of
        Pure x -> (next x, s1)
        Free (ErrorOp err) -> do
            let (res2, s2) = step e s1 e2 
            case res2 of
                Pure y -> (next y, s2)
                Free (ErrorOp _) -> (Free (ErrorOp err), s2)
                _ -> (Free (OneOfOp res1 res2 next), s2)
        _ -> do
            -- Also evaluate e2 if e1 is not yet complete
            let (res2, s2) = step e s1 e2
            case (res1, res2) of
                (_, Pure y) -> (next y, s2)
                _ -> (Free (OneOfOp res1 res2 next), s2)

runEval :: EvalM a -> Either Error a
runEval = runEvalSim' envEmpty stateInitial
  where
    runEvalSim' :: Env -> State -> EvalM a -> Either Error a
    runEvalSim' _ _ (Pure x) = Right x
    runEvalSim' _ _ (Free (ErrorOp err)) = Left err
    runEvalSim' e s next = do
        let (cont, s') = step e s next
        runEvalSim' e s' cont
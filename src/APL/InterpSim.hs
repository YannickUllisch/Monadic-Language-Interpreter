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
--
-- Evaluation of 'BothOp':
--
-- * If either of the nested computations are 'Free (ErrorOp ...)',
--   then propagate that error.
--
-- * If both are 'Pure', then return a pair of the results.
--
-- * Otherwise evaluate both one step.
--
-- Evaluation of 'OneOfOp':
--
-- * If both of the nested computations are 'Free (ErrorOp ...)', then
--   propagate one of the errors.
--
-- * If one is 'Pure', then return that result.
--
-- * Otherwise evaluate both one step.
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
step e s (Free (BothOfOp e1 e2 next)) =
    let (e1', state1) = step e s e1
        (e2', state2) = step e state1 e2
    in case (e1', e2') of
        (Free (ErrorOp err), _) -> (Free (ErrorOp err), state1)
        (_, Free (ErrorOp err)) -> (Free (ErrorOp err), state2)
        (Pure v1, Pure v2)      -> (next (ValTuple [v1, v2]), state2)
        _ -> (Free (BothOfOp e1' e2' next), state2)
step env s (Free (OneOfOp e1 e2 next)) =
    let (e1', state1) = step env s e1
        (e2', state2) = step env state1 e2
    in case (e1', e2') of
        (Free (ErrorOp err1), Free (ErrorOp _)) -> (Free (ErrorOp err1), state2)
        (Pure v1, _) -> (next v1, state2)
        (_, Pure v2) -> (next v2, state2)
        _ -> (Free (OneOfOp e1' e2' next), state2)

runEval :: EvalM a -> Either Error a
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> Either Error a
    runEval' _ _ (Pure x) = Right x
    runEval' _ _ (Free (ErrorOp err)) = Left err
    runEval' e s nxtStep =
        let (cont, s') = step e s nxtStep
        in runEval' e s' cont
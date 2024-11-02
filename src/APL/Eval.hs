module APL.Eval
  ( eval,
  )
where

import APL.AST (Exp (..))
import APL.Monad
import Control.Monad()

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f = evalIntBinOp f'
  where
    f' x y = pure $ f x y

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "If: non-boolean conditional"
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) -> do
      evalStep $ localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (KvPut k_exp v_exp) = do
  k <- eval k_exp
  v <- eval v_exp
  evalKvPut k v
  pure v
eval (KvGet k_exp) = do
  k <- eval k_exp
  evalKvGet k
eval (Tuple exps)
  | length exps == 1 = failure "Single element tuples are not allowed"
  | otherwise = do
      vals <- mapM eval exps
      pure $ ValTuple vals
eval (Project e1 i) = do
  res <- eval e1
  case res of
    ValTuple vals
      | i >= 0 && fromInteger i < length vals -> pure $ vals !! fromInteger i -- Indexing 
      | otherwise -> failure $ "Given index " ++ show i ++ " is out of bounds"
    _ -> failure "Cannot project an element from non tuple type"
eval (ForLoop (name, initial) (cName, bound) body) = do
  v <- eval initial
  n <- eval bound
  case n of
    ValInt nc -> executeLoop 0 v nc
    _ -> failure "Given bound must be of type integer"
  where
    executeLoop i v n
      | i >= n = pure v -- case that loop is finished
      | otherwise = do
        updated <- evalStep $ localEnv (envExtend cName (ValInt i) . envExtend name v) $ eval body
        executeLoop (i + 1) updated n
eval (WhileLoop (name, initialExp) cond body) = do
  vExp <- eval initialExp
  loopWhile vExp
  where
    loopWhile v = do
      condVal <- localEnv (envExtend name v) $ eval cond
      case condVal of
        ValBool True -> do
          updated <- evalStep $ localEnv (envExtend name v) $ eval body
          loopWhile updated
        ValBool False -> pure v
        _ -> failure "Condition must evaluate to boolean"
eval (BothOf e1 e2) = do
  evalBothOf (eval e1) (eval e2)
eval (OneOf e1 e2) = do
  evalOneOf (eval e1) (eval e2)

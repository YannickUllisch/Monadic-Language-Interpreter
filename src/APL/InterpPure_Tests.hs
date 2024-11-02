module APL.InterpPure_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpPure (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v =
  testCase desc $
    runEval (eval e) @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $
    case runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Pure interpreter"
    [ evalTestFail
        "State (unknown key)"
        (KvGet (CstInt 0)),
      --
      -- Should work after task A.
      evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2]),
      --
      -- Should work after Task B.
      evalTest
        "For loop"
        (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
        (ValInt 1024),
      --
      evalTest 
        "For loop2"
        (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))
        (ValInt 32),
      --
      evalTest 
        "While loop decrement"
        (WhileLoop ("x", CstInt 10) (Eql (Var "x") (CstInt 10)) (Sub (Var "x") (CstInt 10))) 
        (ValInt 0),
        --
      evalTest 
        "While loop decrement with tuple"
          (WhileLoop ("x", Tuple [CstInt 10, CstInt 0]) 
                    (Eql (Project (Var "x") 0) (CstInt 10)) 
                    (Tuple [Sub (Project (Var "x") 0) (CstInt 1), CstInt 0]))
          (ValTuple [ValInt 9, ValInt 0]),
      --
      evalTest 
        "While loop"
        (WhileLoop ("x", Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) 
          (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)]))
        (ValTuple [ValInt 1024,ValInt 0]),
      --
      -- Should work after task C.
      evalTest
        "e1 && e2"
        (BothOf (CstInt 0) (CstInt 1))
        (ValTuple [ValInt 0, ValInt 1]),
      --
      evalTest 
        "(1+2) && (3+4)"
        (BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)))
        (ValTuple [ValInt 3,ValInt 7]),
      --
      -- Should work after task C.
      evalTest
        "e1 || e2"
        (OneOf (CstInt 0) (CstInt 1))
        (ValInt 0),
      --
      evalTest 
        "(1+2) || (3+4+5+6)"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3),
      --
      -- Should work after task C.
      evalTest
        "e1 || e2 (first fails)"
        (OneOf (KvGet (CstInt 0)) (CstInt 1))
        (ValInt 1),
      --
      evalTestFail
        "Works only with concurrency 1"
        (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True))),
      --
      evalTestFail
        "Works only with concurrency 2"
        (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
    ]

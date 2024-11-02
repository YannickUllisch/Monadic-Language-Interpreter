module APL.InterpConcurrent_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpConcurrent (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
  res <- runEval $ eval e
  res @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $ do
    res <- runEval (eval e)
    case res of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Concurrent interpreter"
    [
      testGroup
        "Concurrency Operators"
        [
          --  
          evalTestFail
            "BothOf failure in e1"
            (BothOf (Div (CstBool True) (CstInt 0) ) (Div (CstInt 2) (CstInt 2))),
          --
          evalTestFail
            "BothOf failure in e2"
            (BothOf (Div (CstInt 5) (CstInt 5) ) (Div (CstInt 2) (CstInt 0))),
          --
          evalTestFail
            "BothOf failure in e2"
            (BothOf (Div (CstBool True) (CstInt 5) ) (Div (CstBool True) (CstInt 0))),
          --
          evalTestFail
            "OneOf Both Fail"
            (OneOf (Div (CstInt 5) (CstInt 0)) (Div (CstInt 2) (CstInt 0))),  
          --
          evalTest
            "OneOf Left Fails"
            (OneOf (Div (CstInt 5) (CstInt 0)) (Div (CstInt 2) (CstInt 2)))
            (ValInt 1),
          --
          evalTest
            "OneOf Right Fails"
            (OneOf (Div (CstInt 5) (CstInt 5)) (Div (CstInt 2) (CstInt 0)))
            (ValInt 1)
          --
        ],

      evalTest 
        "(1+2) && (3+4)"
        (BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)))
        (ValTuple [ValInt 3,ValInt 7]),
      --
      evalTest 
        "get 0 && put 0 true"
        (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
        (ValTuple [ValBool True,ValBool True]),
      evalTest
        "get 0 + 1 && put 0 2"
        (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
        (ValTuple [ValInt 3,ValInt 2]),
      evalTest
        "put (get 0) 1 && let x = put 0 2 in get 2"
        (BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2))))
        (ValTuple [ValInt 1,ValInt 1]),
      evalTest
        "(1+2) || (3+4+5+6)"
        (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3),
      evalTestFail
        "BothOf failure in e1"
        (BothOf (Div (CstInt 5) (CstInt 0) ) (Div (CstInt 2) (CstInt 2))),
      evalTestFail
        "BothOf failure in e2"
        (BothOf (Div (CstInt 5) (CstInt 5) ) (Div (CstInt 2) (CstInt 0))),
      evalTest 
        "While loop"
        (WhileLoop ("x", Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) 
          (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)]))
        (ValTuple [ValInt 1024,ValInt 0])
    ]

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
        "Concurrency Operator Tests"
        [
          evalTest
            "(1+2) || (3+4+5+6)"
            (OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
            (ValInt 3),
          --
          evalTest 
            "(1+2) && (3+4)"
            (BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)))
            (ValTuple [ValInt 3, ValInt 7]),
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
            "BothOf failure both Fail"
            (BothOf (Div (CstInt 5) (CstBool True) ) (Div (CstInt 2) (CstInt 0))),
          --
          evalTestFail
            "OneOf Both Fail"
            (OneOf (Div (CstInt 5) (CstBool True)) (Div (CstInt 2) (CstInt 0))),  
          --
          evalTest
            "OneOf Left Fails"
            (OneOf (Div (CstInt 5) (CstInt 0)) (Div (CstInt 2) (CstInt 2)))
            (ValInt 1),
          --
          evalTest
            "OneOf Right Fails"
            (OneOf (Div (CstInt 5) (CstInt 5)) (Div (CstInt 2) (CstInt 0)))
            (ValInt 1),
          --
          evalTest
            "OneOf with one Loop and one Constant"
            (OneOf (WhileLoop ("x", CstInt 0) (CstBool True) (Add (Var "x") (CstInt 1)))
                  (CstInt 42))
            (ValInt 42),
          --
          evalTest
            "Concurrent KvPut in BothOf"
            (BothOf (KvPut (CstInt 2) (CstInt 20))
                    (KvGet (CstInt 2)))
            (ValTuple [ValInt 20, ValInt 20])
        ],
      testGroup 
        "Concurrency Put/Get Tests"
        [
          evalTest 
            "get 0 && put 0 true"
            (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
            (ValTuple [ValBool True,ValBool True]),
          --
          evalTest
            "get 0 + 1 && put 0 2"
            (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
            (ValTuple [ValInt 3, ValInt 2]),
          --
          evalTest
            "put (get 0) 1 && let x = put 0 2 in get 2"
            (BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2))))
            (ValTuple [ValInt 1, ValInt 1]),
          --
          evalTest
            "BothOf with KvPut and KvGet"
            (BothOf (KvPut (CstInt 0) (CstInt 100)) (KvGet (CstInt 0)))
            (ValTuple [ValInt 100, ValInt 100]),
          --
          evalTest
            "OneOf with KvGet waiting for KvPut"
            (OneOf (KvGet (CstInt 1)) (KvPut (CstInt 1) (CstInt 50)))
            (ValInt 50)
          --
        ],
      testGroup
        "General Tests (Loops, Tuples, Project etc.)"
        [ 
          evalTest 
            "While loop"
            (WhileLoop ("x", Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) 
              (Tuple [Mul (Project (Var "x") 0) (CstInt 2),Sub (Project (Var "x") 1) (CstInt 1)]))
            (ValTuple [ValInt 1024,ValInt 0]),
          --
          evalTest
            "While loop with tuple operations"
            (WhileLoop ("t", Tuple [CstInt 0, CstInt 4])
                      (Eql (Project (Var "t") 0) (CstInt 0))
                      (Tuple [Add (Project (Var "t") 0) (CstInt 1), Sub (Project (Var "t") 1) (CstInt 1)]))
            (ValTuple [ValInt 1, ValInt 3]),
          --
          evalTest
            "Apply function to projected tuple element"
            (Let "f" (Lambda "x" (Add (Var "x") (CstInt 3)))
              (Apply (Var "f") (Project (Tuple [CstInt 7, CstInt 9]) 1)))
            (ValInt 12),
          --
          evalTest
            "Apply function using Let binding of tuple"
            (Let "t" (Tuple [CstInt 4, CstInt 8])
              (Let "f" (Lambda "x" (Mul (Var "x") (CstInt 2)))
                (Apply (Var "f") (Project (Var "t") 0))))
            (ValInt 8),
          --
          evalTest
          "Nested Let and Apply with tuple projection"
            (Let "a" (Tuple [CstInt 3, CstInt 6])
              (Let "b" (Tuple [CstInt 10, CstInt 20])
                (Apply (Lambda "x" (Add (Project (Var "a") 0) (Var "x")))
                  (Project (Var "b") 1))))
            (ValInt 23),
          --
          evalTestFail
            "Fail case: Let binding and projection with undefined variable"
            (Let "m" (Tuple [CstInt 2, CstInt 4]) (Project (Var "n") 0)),
          --
          evalTest
            "For loop"
            (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
            (ValInt 1024),
          --
          evalTest 
            "For loop 2"
            (ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)))
            (ValInt 32),
          --
          evalTest 
            "While loop decrement"
            (WhileLoop ("x", CstInt 10) (Eql (Var "x") (CstInt 10)) (Sub (Var "x") (CstInt 10))) 
            (ValInt 0),
          --
          evalTest
            "Tuple with different expressions"
            (Tuple [CstInt 1, Add (CstInt 2) (CstInt 3), Mul (CstInt 4) (CstInt 5)])
            (ValTuple [ValInt 1, ValInt 5, ValInt 20])
          --
        ]
    ]
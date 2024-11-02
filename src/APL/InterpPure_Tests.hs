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

-- | Added a new eval function which lets us test for specific error messages
-- needed to evaluate the evaluation order of Tuples.
evalTestError :: String -> Exp -> (String -> Bool) -> TestTree
evalTestError desc e errMatch =
  testCase desc $
    case runEval (eval e) of
      Left err ->
        if errMatch err
          then pure ()
          else assertFailure $ "Expected a matching error but got: " ++ err
      Right v ->
        assertFailure $ "Expected an error but received this value: " ++ show v

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
    [ 
      testGroup
        "Tuple Tests"
        [
          -- Tuple Tests (should work after Task A)
          evalTest
            "Tuple Basic Test"
            (Tuple [CstInt 1, CstInt 2])
            (ValTuple [ValInt 1, ValInt 2]),
          --
          evalTestFail
            "Fail Case: Tuple with second expr failure"
            (Tuple [CstInt 1, Div (CstInt 5) (CstInt 0)]),
          --
          evalTestFail
            "Fail Case: Tuple with first expr failure"
            (Tuple [Div (CstInt 5) (CstInt 0), CstInt 1]),
          --
          evalTestFail
            "Fail Case: Tuple with 1 element)"
            (Tuple [CstInt 1]),
          --
          evalTest
            "Tuple with multiple elements"
            (Tuple [CstInt 1, CstInt 2, CstInt 3])
            (ValTuple [ValInt 1, ValInt 2, ValInt 3]),
          --
          evalTest
            "Tuple Nested Test"
            (Tuple [CstInt 10, Tuple [CstInt 20, CstInt 30]])
            (ValTuple [ValInt 10, ValTuple [ValInt 20, ValInt 30]]),
          --
          evalTest
            "Tuple with empty list"
            (Tuple [])
            (ValTuple []),
          --
          evalTest
            "Tuple with different expressions"
            (Tuple [CstInt 1, Add (CstInt 2) (CstInt 3), Mul (CstInt 4) (CstInt 5)])
            (ValTuple [ValInt 1, ValInt 5, ValInt 20]),
          --
          evalTestFail
            "Fail Case: Nested Tuple with failure inside"
            (Tuple [CstInt 1, Tuple [CstInt 2, Div (CstInt 10) (CstInt 0)]]),
          -- 
          -- Testing Tuple Evaluation order
          evalTestError 
            "Tuple Evaluation Order by multiple failures" 
            (Tuple [CstInt 1, Tuple [CstInt 5], Div (CstInt 5) (CstInt 0)])
            (== "Single element tuples are not allowed"),
          --
          evalTestError 
            "Tuple Evaluation Order by multiple failures 2" 
            (Tuple [Div (CstInt 5) (CstInt 0), Tuple [CstInt 5], CstInt 5])
            (== "Division by zero"),
          --
          -- Projection Tests
          evalTest
            "Projection test: access first element"
            (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) 0)
            (ValInt 7),
          --
          evalTest
            "Projection test: access middle element"
            (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) 1)
            (ValInt 8),
          --
          evalTest
            "Projection test: access last element"
            (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) 2)
            (ValInt 9),
          --
          evalTestFail
            "Projection Fail Case: index out of bounds (negative)"
            (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) (-1)),
          --
          evalTestFail
            "Projection Fail Case: index out of bounds too big"
            (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) 5),
          --
          evalTestFail
            "Projection Fail Case: non-tuple input"
            (Project (CstInt 42) 0),
          --
          evalTest
            "Projection access after brinding to variable"
            (Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0))
            (ValInt 1),
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
            (Let "m" (Tuple [CstInt 2, CstInt 4]) (Project (Var "n") 0))
        ],
      testGroup
        "Get & Put Tests"
        [
          --
          evalTestFail
            "State (unknown key)"
            (KvGet (CstInt 0))
          --
        ],
      testGroup
        "Loop Tests"
        [
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
            (ValTuple [ValInt 1024,ValInt 0])
          --
        ],
      testGroup
        "Concurrency Operator Tests"
        [
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
    ]

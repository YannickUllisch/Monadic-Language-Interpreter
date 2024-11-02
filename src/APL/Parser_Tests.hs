module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ testGroup
        "Tuples"
        [
          parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTestFail "x+",
          parserTest "()" $ Tuple [],
          parserTest "(  )" $ Tuple [],
          parserTest "  (  )  " $ Tuple [],
          parserTest "(x, y)" $ Tuple [Var "x", Var "y"],
          parserTestFail "(x+, y)",
          parserTest "(  x, y   )" $ Tuple [Var "x", Var "y"],
          parserTest "().0" $ Project (Tuple []) 0, -- Evaluation of this would fail, but we still parse it
          parserTest "(x,y,z).4" $ Project (Tuple [Var "x", Var "y", Var "z"]) 4, -- Evaluation should again fail, but we still parse it
          parserTest "(1,2).1 " $ Project (Tuple [CstInt 1,CstInt 2]) 1,
          parserTest "(1,2) .1 " $ Project (Tuple [CstInt 1,CstInt 2]) 1,
          parserTest "(x)" $ Var "x",
          parserTest "let x = (1,2) in x+5" $ Let "x" (Tuple [CstInt 1, CstInt 2]) (Add (Var "x") (CstInt 5)),
          parserTest "let x = (1,2) in x.0" $ Let "x" (Tuple [CstInt 1,CstInt 2]) (Project (Var "x") 0),
          parserTest "(x).0" $ Project (Var "x") 0 -- We can parse it but evaluation will fail.
        ],
      testGroup
        "Loops"
        [
          parserTest "loop x = 1 for i < 5 do x * 2" $ ForLoop ("x",CstInt 1) ("i",CstInt 5) (Mul (Var "x") (CstInt 2)),
          parserTest "loop x = 1 while x == 1 do x" $ WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x"),
          parserTest "loop x = (1,10) while if (x.1 == 0) then false else true do (x.0*2,x.1-1)" $ 
            WhileLoop ("x",Tuple [CstInt 1,CstInt 10]) (If (Eql (Project (Var "x") 1) (CstInt 0)) (CstBool False) (CstBool True)) (Tuple [Mul (Project (Var "x") 0) (CstInt 2), Sub (Project (Var "x") 1) (CstInt 1)])
        ],
      testGroup 
        "Concurrency Operators"
        [
          parserTest "(1+2) && (3+4)" $ BothOf (Add (CstInt 1) (CstInt 2)) (Add (CstInt 3) (CstInt 4)),
          parserTest "(1+2) || (3+4+5+6)" $ OneOf (Add (CstInt 1) (CstInt 2)) (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)),
          parserTest "((2*3) && (4-1))" $ BothOf (Mul (CstInt 2) (CstInt 3)) (Sub (CstInt 4) (CstInt 1)),
          parserTest "(5 || (2+3) * 4)" $ OneOf (CstInt 5) (Mul (Add (CstInt 2) (CstInt 3)) (CstInt 4)),
          parserTest "(10/2) && ((3*2) || (1-1))" $ BothOf (Div (CstInt 10) (CstInt 2)) (OneOf (Mul (CstInt 3) (CstInt 2)) (Sub (CstInt 1) (CstInt 1))),
          parserTest "((1 && 2) || (3 && 4))" $ OneOf (BothOf (CstInt 1) (CstInt 2)) (BothOf (CstInt 3) (CstInt 4)),
          parserTest "((3+4) || (5-2)) && (8*7)" $ BothOf (OneOf (Add (CstInt 3) (CstInt 4)) (Sub (CstInt 5) (CstInt 2))) (Mul (CstInt 8) (CstInt 7)),
          parserTest "42 && ((10/2) || 3)" $ BothOf (CstInt 42) (OneOf (Div (CstInt 10) (CstInt 2)) (CstInt 3)),
          parserTest "(3 || 5) && (4 || 6)" $ BothOf (OneOf (CstInt 3) (CstInt 5)) (OneOf (CstInt 4) (CstInt 6)),
          parserTest "((1+2) && 3) || (4+5)" $ OneOf (BothOf (Add (CstInt 1) (CstInt 2)) (CstInt 3)) (Add (CstInt 4) (CstInt 5))
        ]
    ]
    

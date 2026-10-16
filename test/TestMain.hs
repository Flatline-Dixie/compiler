module Main (main) where

import Arithmetic hiding (main)
import Control.Monad (unless)
import Test.QuickCheck
import System.Exit (exitFailure)

main :: IO ()
main = do
  runRegressionTests
  result <- quickCheckWithResult
    stdArgs {maxSuccess = 500}
    prop_compilerMatchesInterpreter
  unless (isSuccess result) exitFailure

newtype TestExpr = TestExpr Expr
  deriving (Show)

instance Arbitrary TestExpr where
  arbitrary = TestExpr <$> sized (\size -> genExpr size [])

prop_compilerMatchesInterpreter :: TestExpr -> Property
prop_compilerMatchesInterpreter (TestExpr expression) =
  case compile expression of
    Left compileError ->
      counterexample ("compile failed: " ++ show compileError) False
    Right program ->
      let optimized = compile (foldConstants expression)
          matchesInterpreter = run program === eval expression
          optimizedMatchesInterpreter = case optimized of
            Left compileError ->
              counterexample ("optimized compile failed: " ++ show compileError) False
            Right optimizedProgram ->
              run optimizedProgram === eval expression
       in counterexample ("expression: " ++ show expression)
            (matchesInterpreter .&&. optimizedMatchesInterpreter)

genExpr :: Int -> [String] -> Gen Expr
genExpr size variables
  | size <= 0 = genLeaf variables
  | otherwise =
      let smaller = size `div` 2
          recursive constructor =
            constructor <$> genExpr smaller variables
          binary constructor =
            constructor
              <$> genExpr smaller variables
              <*> genExpr smaller variables
          letExpression = do
            name <- elements ["x", "y", "value"]
            value <- genExpr smaller variables
            body <- genExpr smaller (name : variables)
            pure (Let name value body)
          conditional = do
            condition <- genCondition smaller variables
            thenExpr <- genExpr smaller variables
            elseExpr <- genExpr smaller variables
            pure (If condition thenExpr elseExpr)
       in frequency
            ( [ (3, genLeaf variables)
              , (1, recursive Neg)
              , (1, binary Add)
              , (1, binary Sub)
              , (1, binary Mul)
              , (1, binary Div)
              , (1, letExpression)
              , (2, conditional)
              ]
                ++ if null variables
                  then []
                  else [(1, Variable <$> elements variables)]
            )

genLeaf :: [String] -> Gen Expr
genLeaf variables =
  frequency
    ( [ (4, Number <$> elements [minBound, maxBound, -1, 0, 1])
      , (3, Number <$> choose (-1000, 1000))
      ]
        ++ if null variables
          then []
          else [(1, Variable <$> elements variables)]
    )

genCondition :: Int -> [String] -> Gen Condition
genCondition size variables =
  frequency
    [ (1, Truthy <$> genExpr size variables)
    , (2, Compare
        <$> elements
          [ LessThan
          , LessOrEqual
          , Equal
          , NotEqual
          , GreaterOrEqual
          , GreaterThan
          ]
        <*> genExpr size variables
        <*> genExpr size variables
      )
    ]

runRegressionTests :: IO ()
runRegressionTests = do
  assert "out-of-range integer is rejected"
    (case parse (show (toInteger (maxBound :: Int) + 1)) of
      Left parseError -> errorMessage parseError == "integer is out of range"
      Right _ -> False)
  assert "minimum Int literal is accepted"
    (parseAndRun (show (minBound :: Int)) == Right minBound)
  assert "minimum Int divided by -1 is reported without crashing"
    (parseAndRun (show (minBound :: Int) ++ " / -1")
      == Left "runtime error: DivisionOverflow")
  assert "division overflow is returned as a runtime error"
    (run [Push minBound, Push (-1), OpDiv] == Left DivisionOverflow)
  assert "division keeps floor semantics" (parseAndRun "-7 / 2" == Right (-4))
  assert "let-bindings and lexical shadowing work"
    (parseAndRun "let x = 7 in let x = x + 2 in x * 2" == Right 18)
  assert "conditionals and comparisons work"
    (parseAndRun "if 2 + 3 >= 5 then 11 else 0" == Right 11)
  assert "all comparison operators work"
    ( map parseAndRun
        [ "if 1 < 2 then 1 else 0"
        , "if 2 <= 2 then 1 else 0"
        , "if 2 == 2 then 1 else 0"
        , "if 2 != 3 then 1 else 0"
        , "if 3 >= 2 then 1 else 0"
        , "if 3 > 2 then 1 else 0"
        ]
        == replicate 6 (Right 1)
    )
  assert "zero is false and nonzero values are true"
    (parseAndRun "if -3 then 1 else 0" == Right 1
      && parseAndRun "if 0 then 1 else 0" == Right 0)
  assert "unselected conditional branch is not run"
    (parseAndRun "if 1 then 42 else 1 / 0" == Right 42)
  assert "constant folding preserves runtime errors"
    (parseAndRun "1 / 0" == Left "runtime error: DivisionByZero")
  assert "constant arithmetic is folded"
    (foldConstants (Add (Number 2) (Mul (Number 3) (Number 4))) == Number 14)
  assert "unbound variables are rejected"
    (case parse "missing + 1" of
      Left parseError -> "undefined variable" `isPrefixOf` errorMessage parseError
      Right _ -> False)
  where
    assert description condition =
      unless condition (ioError (userError ("failed: " ++ description)))

parseAndRun :: String -> Either String Int
parseAndRun source =
  case parse source of
    Left parseError -> Left ("parse error: " ++ show parseError)
    Right expression ->
      case compile (foldConstants expression) of
        Left compileError -> Left ("compile error: " ++ show compileError)
        Right program ->
          case run program of
            Left runtimeError -> Left ("runtime error: " ++ show runtimeError)
            Right result -> Right result

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix value = take (length prefix) value == prefix

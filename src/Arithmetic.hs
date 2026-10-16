module Arithmetic where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (stripPrefix)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Expr
  = Number Int
  | Variable String
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr
  | If Condition Expr Expr
  deriving (Eq, Show)

data Condition
  = Truthy Expr
  | Compare Comparison Expr Expr
  deriving (Eq, Show)

data Comparison = LessThan | LessOrEqual | Equal | NotEqual | GreaterOrEqual | GreaterThan
  deriving (Eq, Show)

data Instr
  = Push Int
  | Load Int
  | Store Int
  | OpNeg
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpCompare Comparison
  | JumpIfZero Int
  | Jump Int
  deriving (Eq, Show)

data ParseError = ParseError
  { errorOffset :: Int
  , errorMessage :: String
  }
  deriving (Eq, Show)

data CompileError = MissingVariable String
  deriving (Eq, Show)

data RuntimeError
  = StackUnderflow Instr
  | DivisionByZero
  | DivisionOverflow
  | UnboundVariable String
  | UninitializedSlot Int
  | InvalidProgramCounter Int
  | InvalidFinalStack Int
  deriving (Eq, Show)

type ParseResult a = Either ParseError (a, String, Int)

parse :: String -> Either ParseError Expr
parse source = do
  (expr, remaining, offset) <- parseAddSub [] source 0
  let (trailing, trailingOffset) = skipSpaces remaining offset
  case trailing of
    [] -> Right expr
    _ -> Left (ParseError trailingOffset "unexpected input")

parseAddSub :: [String] -> String -> Int -> ParseResult Expr
parseAddSub variables source offset = do
  (first, remaining, nextOffset) <- parseMulDiv variables source offset
  continue first remaining nextOffset
  where
    continue expr remaining currentOffset =
      let (input, inputOffset) = skipSpaces remaining currentOffset
       in case input of
            '+' : rest -> do
              (right, afterRight, afterOffset) <-
                parseMulDiv variables rest (inputOffset + 1)
              continue (Add expr right) afterRight afterOffset
            '-' : rest -> do
              (right, afterRight, afterOffset) <-
                parseMulDiv variables rest (inputOffset + 1)
              continue (Sub expr right) afterRight afterOffset
            _ -> Right (expr, input, inputOffset)

parseMulDiv :: [String] -> String -> Int -> ParseResult Expr
parseMulDiv variables source offset = do
  (first, remaining, nextOffset) <- parseUnary variables source offset
  continue first remaining nextOffset
  where
    continue expr remaining currentOffset =
      let (input, inputOffset) = skipSpaces remaining currentOffset
       in case input of
            '*' : rest -> do
              (right, afterRight, afterOffset) <-
                parseUnary variables rest (inputOffset + 1)
              continue (Mul expr right) afterRight afterOffset
            '/' : rest -> do
              (right, afterRight, afterOffset) <-
                parseUnary variables rest (inputOffset + 1)
              continue (Div expr right) afterRight afterOffset
            _ -> Right (expr, input, inputOffset)

parseUnary :: [String] -> String -> Int -> ParseResult Expr
parseUnary variables source offset =
  let (input, inputOffset) = skipSpaces source offset
   in case input of
        '+' : rest -> parseUnary variables rest (inputOffset + 1)
        '-' : rest ->
          let afterSignOffset = inputOffset + 1
              (afterSign, numberOffset) = skipSpaces rest afterSignOffset
           in case afterSign of
                first : _
                  | isAsciiDigit first -> parseSignedInteger afterSign numberOffset
                _ -> do
                  (expr, remaining, nextOffset) <-
                    parseUnary variables rest afterSignOffset
                  Right (Neg expr, remaining, nextOffset)
        _ -> parseAtom variables input inputOffset
  where
    parseSignedInteger input numberOffset =
      let (digits, remaining) = span isAsciiDigit input
          nextOffset = numberOffset + length digits
       in case readMaybe digits :: Maybe Integer of
            Just unsignedValue ->
              let value = negate unsignedValue
               in if value >= toInteger (minBound :: Int)
                    then Right (Number (fromInteger value), remaining, nextOffset)
                    else Left (ParseError numberOffset "integer is out of range")
            Nothing -> Left (ParseError numberOffset "integer is out of range")

parseAtom :: [String] -> String -> Int -> ParseResult Expr
parseAtom variables source offset
  | Just (rest, nextOffset) <- consumeKeyword "let" source offset =
      parseLet variables rest nextOffset
  | Just (rest, nextOffset) <- consumeKeyword "if" source offset =
      parseIf variables rest nextOffset
  | otherwise = case source of
      '(' : rest -> do
        (expr, remaining, nextOffset) <- parseAddSub variables rest (offset + 1)
        let (input, inputOffset) = skipSpaces remaining nextOffset
        case input of
          ')' : afterParen -> Right (expr, afterParen, inputOffset + 1)
          _ -> Left (ParseError inputOffset "expected ')'")
      first : _
        | isAsciiDigit first ->
            let (digits, remaining) = span isAsciiDigit source
                nextOffset = offset + length digits
             in case readMaybe digits :: Maybe Integer of
                  Just value
                    | value <= toInteger (maxBound :: Int) ->
                        Right (Number (fromInteger value), remaining, nextOffset)
                  _ -> Left (ParseError offset "integer is out of range")
        | isIdentifierStart first ->
              let (name, remaining) = span isIdentifierCharacter source
                  nextOffset = offset + length name
               in if name `elem` variables
                    then Right (Variable name, remaining, nextOffset)
                    else Left (ParseError offset ("undefined variable '" ++ name ++ "'"))
      _ -> Left (ParseError offset "expected an integer, variable, 'let', 'if', or '('")

parseLet :: [String] -> String -> Int -> ParseResult Expr
parseLet variables source offset =
  let (input, inputOffset) = skipSpaces source offset
      (name, afterName) =
        case input of
          first : _ | isIdentifierStart first -> span isIdentifierCharacter input
          _ -> ("", input)
      nameOffset = inputOffset + length name
      (afterNameSpaces, equalsOffset) = skipSpaces afterName nameOffset
   in case (name, afterNameSpaces) of
        ("", _) -> Left (ParseError inputOffset "expected a variable name after 'let'")
        ("let", _) -> Left (ParseError inputOffset "'let' is a reserved word")
        ("if", _) -> Left (ParseError inputOffset "'if' is a reserved word")
        (_, '=' : rest)
          | not (startsWith "=" rest) -> do
              (value, afterValue, afterValueOffset) <-
                parseAddSub variables rest (equalsOffset + 1)
              case consumeKeyword "in" afterValue afterValueOffset of
                Nothing -> Left (ParseError afterValueOffset "expected 'in'")
                Just (bodySource, bodyOffset) -> do
                  (body, remaining, nextOffset) <-
                    parseAddSub (name : variables) bodySource bodyOffset
                  Right (Let name value body, remaining, nextOffset)
        _ -> Left (ParseError equalsOffset "expected '=' after the variable name")

parseIf :: [String] -> String -> Int -> ParseResult Expr
parseIf variables source offset = do
  (left, afterLeft, afterLeftOffset) <- parseAddSub variables source offset
  let (input, inputOffset) = skipSpaces afterLeft afterLeftOffset
      comparison = parseComparison input
  (condition, afterCondition, afterConditionOffset) <- case comparison of
    Nothing -> Right (Truthy left, input, inputOffset)
    Just (operator, rest, operatorLength) -> do
      (right, remaining, nextOffset) <-
        parseAddSub variables rest (inputOffset + operatorLength)
      Right (Compare operator left right, remaining, nextOffset)
  case consumeKeyword "then" afterCondition afterConditionOffset of
    Nothing -> Left (ParseError afterConditionOffset "expected 'then'")
    Just (thenSource, thenOffset) -> do
      (thenExpr, afterThen, afterThenOffset) <-
        parseAddSub variables thenSource thenOffset
      case consumeKeyword "else" afterThen afterThenOffset of
        Nothing -> Left (ParseError afterThenOffset "expected 'else'")
        Just (elseSource, elseOffset) -> do
          (elseExpr, remaining, nextOffset) <-
            parseAddSub variables elseSource elseOffset
          Right (If condition thenExpr elseExpr, remaining, nextOffset)

parseComparison :: String -> Maybe (Comparison, String, Int)
parseComparison input =
  firstMatch
    [ ("<=", LessOrEqual)
    , (">=", GreaterOrEqual)
    , ("==", Equal)
    , ("!=", NotEqual)
    , ("<", LessThan)
    , (">", GreaterThan)
    ]
  where
    firstMatch [] = Nothing
    firstMatch ((operator, comparison) : rest) =
      case stripPrefix operator input of
        Just remaining -> Just (comparison, remaining, length operator)
        Nothing -> firstMatch rest

consumeKeyword :: String -> String -> Int -> Maybe (String, Int)
consumeKeyword keyword source offset =
  case stripPrefix keyword source of
    Just remaining
      | not (maybe False isIdentifierCharacter (safeHead remaining)) ->
          Just (remaining, offset + length keyword)
    _ -> Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (value : _) = Just value

startsWith :: String -> String -> Bool
startsWith prefix source = maybe False (const True) (stripPrefix prefix source)

skipSpaces :: String -> Int -> (String, Int)
skipSpaces source offset =
  let (spaces, remaining) = span isSpace source
   in (remaining, offset + length spaces)

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

isIdentifierStart :: Char -> Bool
isIdentifierStart character = isAlpha character || character == '_'

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
  isAlphaNum character || character == '_'

foldConstants :: Expr -> Expr
foldConstants expression = case expression of
  Number _ -> expression
  Variable _ -> expression
  Neg expr -> case foldConstants expr of
    Number value -> Number (negate value)
    folded -> Neg folded
  Add left right -> foldBinary Add (+) left right
  Sub left right -> foldBinary Sub (-) left right
  Mul left right -> foldBinary Mul (*) left right
  Div left right ->
    let foldedLeft = foldConstants left
        foldedRight = foldConstants right
     in case (foldedLeft, foldedRight) of
          (Number dividend, Number divisor)
            | divisor /= 0
                && not (dividend == minBound && divisor == -1) ->
                  Number (dividend `div` divisor)
          _ -> Div foldedLeft foldedRight
  Let name value body ->
    Let name (foldConstants value) (foldConstants body)
  If condition thenExpr elseExpr ->
    let foldedCondition = foldCondition condition
        foldedThen = foldConstants thenExpr
        foldedElse = foldConstants elseExpr
     in case conditionValue foldedCondition of
          Just True -> foldedThen
          Just False -> foldedElse
          Nothing -> If foldedCondition foldedThen foldedElse
  where
    foldBinary constructor operation left right =
      let foldedLeft = foldConstants left
          foldedRight = foldConstants right
       in case (foldedLeft, foldedRight) of
            (Number leftValue, Number rightValue) ->
              Number (operation leftValue rightValue)
            _ -> constructor foldedLeft foldedRight

foldCondition :: Condition -> Condition
foldCondition condition = case condition of
  Truthy expr -> Truthy (foldConstants expr)
  Compare comparison left right ->
    Compare comparison (foldConstants left) (foldConstants right)

conditionValue :: Condition -> Maybe Bool
conditionValue condition = case condition of
  Truthy (Number value) -> Just (value /= 0)
  Compare comparison (Number left) (Number right) ->
    Just (compareValues comparison left right)
  _ -> Nothing

compile :: Expr -> Either CompileError [Instr]
compile expression = do
  (instructions, _) <- compileExpr Map.empty 0 expression
  Right instructions

compileExpr :: Map String Int -> Int -> Expr -> Either CompileError ([Instr], Int)
compileExpr _ next (Number value) = Right ([Push value], next)
compileExpr variables next (Variable name) =
  case Map.lookup name variables of
    Just slot -> Right ([Load slot], next)
    Nothing -> Left (MissingVariable name)
compileExpr variables next (Neg expr) = do
  (instructions, nextSlot) <- compileExpr variables next expr
  Right (instructions ++ [OpNeg], nextSlot)
compileExpr variables next (Add left right) =
  compileBinary variables next left right OpAdd
compileExpr variables next (Sub left right) =
  compileBinary variables next left right OpSub
compileExpr variables next (Mul left right) =
  compileBinary variables next left right OpMul
compileExpr variables next (Div left right) =
  compileBinary variables next left right OpDiv
compileExpr variables next (Let name value body) = do
  let slot = next
  (valueCode, nextAfterValue) <- compileExpr variables (slot + 1) value
  (bodyCode, nextAfterBody) <-
    compileExpr (Map.insert name slot variables) nextAfterValue body
  Right (valueCode ++ [Store slot] ++ bodyCode, nextAfterBody)
compileExpr variables next (If condition thenExpr elseExpr) = do
  (conditionCode, nextAfterCondition) <-
    compileCondition variables next condition
  (thenCode, nextAfterThen) <-
    compileExpr variables nextAfterCondition thenExpr
  (elseCode, nextAfterElse) <-
    compileExpr variables nextAfterThen elseExpr
  let branchCode =
        conditionCode
          ++ [JumpIfZero (length thenCode + 1)]
          ++ thenCode
          ++ [Jump (length elseCode)]
          ++ elseCode
  Right (branchCode, nextAfterElse)

compileBinary
  :: Map String Int
  -> Int
  -> Expr
  -> Expr
  -> Instr
  -> Either CompileError ([Instr], Int)
compileBinary variables next left right operator = do
  (leftCode, nextAfterLeft) <- compileExpr variables next left
  (rightCode, nextAfterRight) <- compileExpr variables nextAfterLeft right
  Right (leftCode ++ rightCode ++ [operator], nextAfterRight)

compileCondition
  :: Map String Int
  -> Int
  -> Condition
  -> Either CompileError ([Instr], Int)
compileCondition variables next condition = case condition of
  Truthy expr -> compileExpr variables next expr
  Compare comparison left right -> do
    (leftCode, nextAfterLeft) <- compileExpr variables next left
    (rightCode, nextAfterRight) <-
      compileExpr variables nextAfterLeft right
    Right (leftCode ++ rightCode ++ [OpCompare comparison], nextAfterRight)

run :: [Instr] -> Either RuntimeError Int
run program = execute 0 [] Map.empty
  where
    programLength = length program
    indexedProgram = IntMap.fromList (zip [0 ..] program)

    execute pc stack slots
      | pc == programLength = finish stack
      | pc < 0 || pc > programLength = Left (InvalidProgramCounter pc)
      | otherwise =
          case programAt pc of
            Nothing -> Left (InvalidProgramCounter pc)
            Just instruction -> step pc instruction stack slots

    programAt pc = IntMap.lookup pc indexedProgram

    step pc instruction stack slots = case instruction of
      Push value -> execute (pc + 1) (value : stack) slots
      Load slot -> case Map.lookup slot slots of
        Just value -> execute (pc + 1) (value : stack) slots
        Nothing -> Left (UninitializedSlot slot)
      Store slot -> case stack of
        value : rest ->
          execute (pc + 1) rest (Map.insert slot value slots)
        _ -> Left (StackUnderflow instruction)
      OpNeg -> case stack of
        value : rest -> execute (pc + 1) (negate value : rest) slots
        _ -> Left (StackUnderflow instruction)
      OpAdd -> runBinary pc instruction (+) stack slots
      OpSub -> runBinary pc instruction (-) stack slots
      OpMul -> runBinary pc instruction (*) stack slots
      OpDiv -> case stack of
        divisor : dividend : rest
          | divisor == 0 -> Left DivisionByZero
          | dividend == minBound && divisor == -1 -> Left DivisionOverflow
          | otherwise ->
              execute (pc + 1) (dividend `div` divisor : rest) slots
        _ -> Left (StackUnderflow instruction)
      OpCompare comparison -> case stack of
        right : left : rest ->
          let result = fromEnum (compareValues comparison left right)
           in execute (pc + 1) (result : rest) slots
        _ -> Left (StackUnderflow instruction)
      JumpIfZero offset -> case stack of
        value : rest ->
          let nextPc =
                if value == 0 then pc + 1 + offset else pc + 1
           in execute nextPc rest slots
        _ -> Left (StackUnderflow instruction)
      Jump offset -> execute (pc + 1 + offset) stack slots

    runBinary pc instruction operation stack slots = case stack of
      right : left : rest ->
        execute (pc + 1) (operation left right : rest) slots
      _ -> Left (StackUnderflow instruction)

    finish stack = case stack of
      [result] -> Right result
      _ -> Left (InvalidFinalStack (length stack))

eval :: Expr -> Either RuntimeError Int
eval = evalWith Map.empty

evalWith :: Map String Int -> Expr -> Either RuntimeError Int
evalWith _ (Number value) = Right value
evalWith variables (Variable name) =
  maybe (Left (UnboundVariable name)) Right (Map.lookup name variables)
evalWith variables (Neg expr) = negate <$> evalWith variables expr
evalWith variables (Add left right) =
  evalBinary variables (+) left right
evalWith variables (Sub left right) =
  evalBinary variables (-) left right
evalWith variables (Mul left right) =
  evalBinary variables (*) left right
evalWith variables (Div left right) = do
  dividend <- evalWith variables left
  divisor <- evalWith variables right
  if divisor == 0
    then Left DivisionByZero
    else if dividend == minBound && divisor == -1
      then Left DivisionOverflow
      else Right (dividend `div` divisor)
evalWith variables (Let name value body) = do
  evaluatedValue <- evalWith variables value
  evalWith (Map.insert name evaluatedValue variables) body
evalWith variables (If condition thenExpr elseExpr) = do
  result <- evalCondition variables condition
  evalWith variables (if result then thenExpr else elseExpr)

evalBinary
  :: Map String Int
  -> (Int -> Int -> Int)
  -> Expr
  -> Expr
  -> Either RuntimeError Int
evalBinary variables operation left right = do
  leftValue <- evalWith variables left
  rightValue <- evalWith variables right
  Right (operation leftValue rightValue)

evalCondition :: Map String Int -> Condition -> Either RuntimeError Bool
evalCondition variables condition = case condition of
  Truthy expr -> (/= 0) <$> evalWith variables expr
  Compare comparison left right -> do
    leftValue <- evalWith variables left
    rightValue <- evalWith variables right
    Right (compareValues comparison leftValue rightValue)

compareValues :: Comparison -> Int -> Int -> Bool
compareValues comparison left right = case comparison of
  LessThan -> left < right
  LessOrEqual -> left <= right
  Equal -> left == right
  NotEqual -> left /= right
  GreaterOrEqual -> left >= right
  GreaterThan -> left > right

main :: IO ()
main = do
  arguments <- getArgs
  source <- case arguments of
    [] -> getContents
    _ -> pure (unwords arguments)
  case parse source of
    Left parseError -> do
      hPutStrLn stderr
        ("Parse error at column " ++ show (errorOffset parseError + 1)
          ++ ": " ++ errorMessage parseError)
      exitFailure
    Right expr ->
      case compile (foldConstants expr) of
        Left compileError -> do
          hPutStrLn stderr ("Compile error: " ++ show compileError)
          exitFailure
        Right program ->
          case run program of
            Left runtimeError -> do
              hPutStrLn stderr ("Runtime error: " ++ show runtimeError)
              exitFailure
            Right result -> do
              putStrLn ("Bytecode: " ++ show program)
              putStrLn ("Result: " ++ show result)

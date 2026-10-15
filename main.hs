module Main where

import Control.Monad (foldM)
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

data Expr
  = Number Int
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

data Instr
  = Push Int
  | OpNeg
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  deriving (Eq, Show)

data ParseError = ParseError
  { errorOffset :: Int
  , errorMessage :: String
  }
  deriving (Eq, Show)

data RuntimeError
  = StackUnderflow Instr
  | DivisionByZero
  | InvalidFinalStack Int
  deriving (Eq, Show)

type ParseResult a = Either ParseError (a, String, Int)

compile :: Expr -> [Instr]
compile expr = compileInto expr []

compileInto :: Expr -> [Instr] -> [Instr]
compileInto (Number n) rest = Push n : rest
compileInto (Neg expr) rest = compileInto expr (OpNeg : rest)
compileInto (Add left right) rest =
  compileInto left (compileInto right (OpAdd : rest))
compileInto (Sub left right) rest =
  compileInto left (compileInto right (OpSub : rest))
compileInto (Mul left right) rest =
  compileInto left (compileInto right (OpMul : rest))
compileInto (Div left right) rest =
  compileInto left (compileInto right (OpDiv : rest))

run :: [Instr] -> Either RuntimeError Int
run program = do
  stack <- foldM step [] program
  case stack of
    [result] -> Right result
    _ -> Left (InvalidFinalStack (length stack))
  where
    step stack instruction = case instruction of
      Push n -> Right (n : stack)
      OpNeg -> case stack of
        value : rest -> Right ((-value) : rest)
        _ -> Left (StackUnderflow instruction)
      OpAdd -> binary instruction (+) stack
      OpSub -> binary instruction (-) stack
      OpMul -> binary instruction (*) stack
      OpDiv -> case stack of
        0 : _ : _ -> Left DivisionByZero
        divisor : dividend : rest ->
          Right ((dividend `div` divisor) : rest)
        _ -> Left (StackUnderflow instruction)

    binary instruction operation stack = case stack of
      right : left : rest -> Right (operation left right : rest)
      _ -> Left (StackUnderflow instruction)

parse :: String -> Either ParseError Expr
parse source = do
  (expr, remaining, offset) <- parseAddSub source 0
  let (trailing, trailingOffset) = skipSpaces remaining offset
  case trailing of
    [] -> Right expr
    _ -> Left (ParseError trailingOffset "unexpected input")

parseAddSub :: String -> Int -> ParseResult Expr
parseAddSub source offset = do
  (first, remaining, nextOffset) <- parseMulDiv source offset
  continue first remaining nextOffset
  where
    continue expr remaining currentOffset =
      let (input, inputOffset) = skipSpaces remaining currentOffset
       in case input of
            '+' : rest -> do
              (right, afterRight, afterOffset) <-
                parseMulDiv rest (inputOffset + 1)
              continue (Add expr right) afterRight afterOffset
            '-' : rest -> do
              (right, afterRight, afterOffset) <-
                parseMulDiv rest (inputOffset + 1)
              continue (Sub expr right) afterRight afterOffset
            _ -> Right (expr, input, inputOffset)

parseMulDiv :: String -> Int -> ParseResult Expr
parseMulDiv source offset = do
  (first, remaining, nextOffset) <- parseUnary source offset
  continue first remaining nextOffset
  where
    continue expr remaining currentOffset =
      let (input, inputOffset) = skipSpaces remaining currentOffset
       in case input of
            '*' : rest -> do
              (right, afterRight, afterOffset) <-
                parseUnary rest (inputOffset + 1)
              continue (Mul expr right) afterRight afterOffset
            '/' : rest -> do
              (right, afterRight, afterOffset) <-
                parseUnary rest (inputOffset + 1)
              continue (Div expr right) afterRight afterOffset
            _ -> Right (expr, input, inputOffset)

parseUnary :: String -> Int -> ParseResult Expr
parseUnary source offset =
  let (input, inputOffset) = skipSpaces source offset
   in case input of
        '+' : rest -> parseUnary rest (inputOffset + 1)
        '-' : rest -> do
          (expr, remaining, nextOffset) <- parseUnary rest (inputOffset + 1)
          Right (Neg expr, remaining, nextOffset)
        _ -> parseAtom input inputOffset

parseAtom :: String -> Int -> ParseResult Expr
parseAtom source offset = case source of
  '(' : rest -> do
    (expr, remaining, nextOffset) <- parseAddSub rest (offset + 1)
    let (input, inputOffset) = skipSpaces remaining nextOffset
    case input of
      ')' : afterParen -> Right (expr, afterParen, inputOffset + 1)
      _ -> Left (ParseError inputOffset "expected ')'")
  first : _
    | isAsciiDigit first ->
        let (digits, remaining) = span isAsciiDigit source
            nextOffset = offset + length digits
         in case readMaybe digits of
              Just value -> Right (Number value, remaining, nextOffset)
              Nothing -> Left (ParseError offset "integer is out of range")
  _ -> Left (ParseError offset "expected an integer or '('")

skipSpaces :: String -> Int -> (String, Int)
skipSpaces source offset =
  let (spaces, remaining) = span isSpace source
   in (remaining, offset + length spaces)

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

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
    Right expr -> do
      let program = compile expr
      case run program of
        Left runtimeError -> do
          hPutStrLn stderr ("Runtime error: " ++ show runtimeError)
          exitFailure
        Right result -> do
          putStrLn ("Bytecode: " ++ show program)
          putStrLn ("Result: " ++ show result)

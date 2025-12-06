module Main where

import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (transpose)
import Data.List.HT (chop)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    parse,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String
type ParseError = ParseErrorBundle String Void
data Op = Add | Mult deriving (Eq, Show)
type Value = Int
type Exercise = (Either Value Op)

exercise :: Parser Exercise
exercise = try (Right <$> op) <|> (Left <$> value)
  where
    value = read <$> some digitChar
    op = (char '+' $> Add) <|> (char '*' $> Mult)

parser :: Parser [Exercise]
parser = some (try (space *> exercise)) <* (space <* eof) -- Force consumption till end

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

execute :: [Exercise] -> Value
execute l = sum $ map (\a -> foldr1 (calculate a) values) ops -- unsafe if values is empty
  where
    (values, ops) = partitionEithers l
    calculate Mult a b = a * b
    calculate Add a b = a + b

main :: IO ()
main = do
  let c = lines <$> readFile "day6/input.txt"
  rawContents <- c
  contents <- map unwords . transpose . map words <$> c

  let inputs = map (parse parser "") contents

      nRows = length rawContents
      pivoted = map unwords . chop (replicate nRows ' ' ==) . transpose $ rawContents
      inputs2 = map (parse parser "") pivoted
  -- print contents

  validInput <- validateParse inputs
  validInput2 <- validateParse inputs2

  let res = sum $ map execute validInput
      res2 = sum $ map execute validInput2
  -- print validInput

  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

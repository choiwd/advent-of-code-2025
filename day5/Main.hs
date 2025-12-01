module Main where

import Data.Either (partitionEithers)
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
import Text.Megaparsec.Char (char, digitChar)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

data Range = Range Int Int deriving (Show, Eq)

newtype Ingredient = Ingredient Int deriving (Show, Eq)

type Database = (Either Range Ingredient)

toDatabase :: Parser Database
toDatabase = try (Left <$> range) <|> (Right <$> ingredient)
  where
    ingredient = Ingredient . read <$> some digitChar
    range = (Range . read <$> some digitChar) <*> (char '-' *> (read <$> some digitChar))

parser :: Parser Database
parser = toDatabase <* eof -- Force consumption till end

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

isFresh :: Ingredient -> [Range] -> Bool
isFresh i = any (isInRange i)
  where
    isInRange (Ingredient x) (Range y z) = y <= x && x <= z

mergeRanges :: [Range] -> [Range]
mergeRanges (x : xs)
  | not hasOverlap = newHead : mergeRanges newTail
  | otherwise = mergeRanges (newHead : newTail)
  where
    (hasOverlap, newHead, newTail) = foldl' f (False, x, []) xs
    f (change, acc, l) range
      | overlaps x range = (True, merge acc range, l)
      | otherwise = (change, acc, l ++ [range])

    overlaps (Range a b) (Range c d) = (a <= c && c <= b) || (a <= d && d <= b) || (c <= a && b <= d)
    merge (Range a b) (Range c d) = Range (min a c) (max b d)
mergeRanges _ = []

main :: IO ()
main = do
  contents <- filter (not . null) . lines <$> readFile "day5/input.txt"
  let inputs = map (parse parser "") contents
  -- print contents
  -- print inputs
  validInput <- validateParse inputs

  let (ranges, ingredients) = partitionEithers validInput
      res = length . filter id $ map (`isFresh` ranges) ingredients

      nItemsInside (Range a b) = b - a + 1
      res2 = sum $ map nItemsInside (mergeRanges ranges)

  -- print (partitionEithers validInput)
  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

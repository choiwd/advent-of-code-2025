module Main where

import Data.List (isPrefixOf)
import Text.Megaparsec
  ( Parsec,
    parseMaybe,
    sepBy,
    some,
  )
import Text.Megaparsec.Char (char, digitChar)

type Parser = Parsec () String

data Range = Range String String deriving (Show)

range :: Parser Range
range = Range <$> some digitChar <*> (char '-' *> some digitChar)

parser :: Parser [Range]
parser = sepBy range (char ',')

isRepeatedTwice :: String -> Bool
isRepeatedTwice = isRepeatedTwice_ 1
  where
    isRepeatedTwice_ l s
      | l > remaining = False
      | div (totalLength + 1) 2 == l && isPrefixOf tail_ (cycle prefix) = True
      | otherwise = isRepeatedTwice_ (l + 1) s
      where
        (prefix, tail_) = splitAt l s
        totalLength = length s
        remaining = totalLength - l

isRepeatedNTimes :: String -> Bool
isRepeatedNTimes = isRepeatedNTimes_ 1
  where
    isRepeatedNTimes_ l s
      | l > remaining = False
      | mod totalLength l == 0 && isPrefixOf tail_ (cycle prefix) = True
      | otherwise = isRepeatedNTimes_ (l + 1) s
      where
        (prefix, tail_) = splitAt l s
        totalLength = length s
        remaining = totalLength - l

makeList :: Range -> [Int]
makeList (Range a b) = [read a .. read b]

main :: IO ()
main = do
  contents <- readFile "day2/input.txt"
  let inputs = parseMaybe parser contents
      res :: Maybe Int = (foldl' f 0) <$> inputs
      f acc r = acc + sum [i | i <- makeList r, isRepeatedTwice (show i)]

      res2 :: Maybe Int = (foldl' f2 0) <$> inputs
      f2 acc r = acc + sum [i | i <- makeList r, isRepeatedNTimes (show i)]

  -- print inputs
  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

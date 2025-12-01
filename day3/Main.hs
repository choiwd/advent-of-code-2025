module Main where

import Data.List (maximumBy, sortOn, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    many,
    parse,
  )
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

parser :: Parser [Int]
parser = many (read <$> pure <$> digitChar) <* eof -- Force consumption till end

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

toNumber :: (Num a) => [a] -> a
toNumber = foldl' (\x b -> x * 10 + b) 0

joltage1 :: (Num a, Ord a) => [a] -> Maybe [a]
joltage1 = listToMaybe . reverse . sortOn toNumber . catMaybes . map makePairs . tails
  where
    makePairs (x : xs)
      | null xs = Nothing
      | otherwise = Just [x, maximum xs]
    makePairs _ = Nothing

slice :: Int -> Int -> [a] -> [a]
slice start end l = [i | (j, i) <- zip [0 ..] l, start <= j, j <= end]

joltageN :: (Ord a, Show a) => Int -> [a] -> Maybe [a]
joltageN n l = if length l >= n then Just (f [] start end) else Nothing
  where
    leftmostMax j = maximumBy (\(a, _) (b, _) -> compare a b) (reverse j)
    indexed = zip l [0 ..]
    (start, end) = splitAt (length l - n) indexed
    f acc left (r : rs) =
      if valueL >= valueR then f (acc ++ [valueL]) (slice (posL + 1) posR indexed) rs else acc ++ (valueR : map fst rs)
      where
        (valueL, posL) = leftmostMax left
        (valueR, posR) = r
    f acc _ _ = acc

main :: IO ()
main = do
  contents <- lines <$> readFile "day3/input.txt"
  let inputs = map (parse parser "") contents
  -- print inputs
  -- print contents
  validInput <- validateParse inputs

  let res = foldl' (\a b -> a + toNumber b) 0 . catMaybes $ map joltage1 validInput
      res2 = foldl' (\a b -> a + toNumber b) 0 . catMaybes $ map (joltageN 12) validInput

  -- print validInput
  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

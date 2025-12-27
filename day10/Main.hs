module Main where

import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (find, sortOn, subsequences)
import Data.List.HT (tails)
import Data.Ord (Down (Down))
import Data.Void (Void)
import Debug.Trace (traceShow, traceShowId)
import Text.Megaparsec
  ( MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    parse,
    sepBy1,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (char, digitChar, space)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

newtype Lights = Lights [Bool] deriving (Eq, Show)

newtype Buttom = Buttom [Int] deriving (Eq, Show)

newtype Joltage = Joltage [Int] deriving (Eq, Show)

data ManualEntry = ME Lights [Buttom] Joltage deriving (Eq, Show)

lights :: Parser Lights
lights = do
  _ <- char '['
  l <- some (try (char '.' $> False) <|> (char '#' $> True))
  _ <- char ']'
  pure $ Lights l

buttons :: Parser [Buttom]
buttons = do
  let buttom :: Parser [Int] = char '(' *> sepBy1 (read <$> some digitChar) (char ',') <* char ')'
  some (Buttom <$> buttom <* space)

joltage :: Parser Joltage
joltage = do
  _ <- char '{'
  l <- sepBy1 (read <$> some digitChar) (char ',')
  _ <- char '}'
  pure $ Joltage l

manualEntry :: Parser ManualEntry
manualEntry = ME <$> (lights <* space) <*> (buttons <* space) <*> joltage <* eof -- Force consumption till end

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

applyCombination :: [Buttom] -> Lights -> Lights
applyCombination bs ls = foldl' f ls bs
  where
    f acc (Buttom b) = foldr flipPos acc b
    flipPos pos (Lights l) = Lights $ foldr g [] (zip l [0 ..])
      where
        g (value, index) acc = if index == pos then not value : acc else value : acc

shortestCombination :: ManualEntry -> Maybe Int
shortestCombination (ME ls buttons_ _) = length . fst <$> x
  where
    combinations = sortOn length . subsequences
    isFullyOff (Lights l) = all not l -- All False
    x = find (isFullyOff . snd) (map (\c -> (c, applyCombination c ls)) $ combinations buttons_)

safeApplyJoltage :: Buttom -> Joltage -> (Joltage, Int) -> (Joltage, Int)
safeApplyJoltage (Buttom b) (Joltage jRef) (acc, initialCounter) = go acc initialCounter
  where
    isSafe (Joltage x) = and $ zipWith (<=) x jRef
    buttomAsJoltage = [if i `elem` b then 1 else 0 | (_, i) <- zip jRef [0 ..]]
    applyJoltage (Joltage x) = Joltage $ zipWith (+) buttomAsJoltage x
    go x counter
      | isSafe (applyJoltage x) = go (applyJoltage x) (counter + 1)
      | otherwise = (x, counter)

leastPressesJoltage :: ManualEntry -> (Joltage, Int)
leastPressesJoltage (ME _ buttons_ refJoltage@(Joltage j)) = x
  where
    zeroJoltage = Joltage [0 | _ <- j]
    sortedButtons = sortOn (\(Buttom b) -> length b) buttons_
    x = foldr f (zeroJoltage, 0) sortedButtons
    f b acc@(jolt, _)
      | jolt == refJoltage = acc
      | otherwise = safeApplyJoltage b refJoltage acc

main :: IO ()
main = do
  contents <- lines <$> readFile "day10/example.txt"

  let inputs = map (parse manualEntry "") contents

  -- print contents

  validInput <- validateParse inputs

  let res = sum <$> mapM shortestCombination validInput
      res2 = map leastPressesJoltage validInput
  -- print validInput

  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

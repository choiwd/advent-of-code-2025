module Main where

import Data.Array.IArray (Array, listArray, (!?), bounds, elems, indices, (//))
import Data.Functor (($>))
import Data.List.HT (allEqual, sliceVertical)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof),
    ParseErrorBundle,
    Parsec,
    many,
    parse,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

data Position = Empty | PaperRoll deriving (Eq)

instance Show Position where
  show :: Position -> String
  show PaperRoll = "@"
  show Empty = "."

parser :: Parser [Position]
parser = many (try ((char '.' $> Empty) <|> char '@' $> PaperRoll)) <* eof -- Force consumption till end

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

toMatrix :: (Show a) => [[a]] -> Maybe (Array (Int, Int) a)
toMatrix l =
  case l of
    x : _ ->
      if allEqual (map length l)
        then pure . listArray ((0, 0), (length x - 1, length l - 1)) $ concat l
        else Nothing
    [] -> Nothing

prettyPrint :: (Show a) => Array (Int, Int) a -> IO ()
prettyPrint a = do
  let ((i, _), (i2, _)) = bounds a
      e = sliceVertical (i2 - i + 1) (elems a)
  mapM_ print e

prettyPrint' :: (Show a) => Maybe (Array (Int, Int) a) -> IO ()
prettyPrint' x =
  case x of
    Just a -> prettyPrint a
    _ -> print "Nothing"

-- Dirty trick: If the given position is not a paper roll then 9
countNeighbors :: (Int, Int) -> Array (Int, Int) Position -> Int
countNeighbors (p1, p2) a = if isPaperRoll then sum (map (collapse . (a !?) . getPos) x) else 9
  where
    l = [-1, 0, 1]
    x = filter (\i -> i /= (0, 0)) (map (,) l <*> l)
    getPos (xi, xj) = (xi + p1, xj + p2)
    collapse item =
      case item of
        Just PaperRoll -> 1
        _ -> 0
    isPaperRoll =
      case a !? (p1, p2) of
        Just PaperRoll -> True
        _ -> False

removePaperRolls :: [(Int, Int)] -> Array (Int, Int) Position -> Array (Int, Int) Position
removePaperRolls r a = a // [(i, Empty) | i <- r]

removeAllPossiblePaperRolls :: Array (Int, Int) Position -> (Array (Int, Int) Position, Int)
removeAllPossiblePaperRolls x = go 0 (makeRemoveList x) x
  where
    makeRemoveList l = [i | i <- indices l, countNeighbors i l < 4]
    go acc removeList a
      | null removeList = (a, acc)
      | otherwise = go (acc + length removeList) (makeRemoveList newList) newList
      where
        newList = removePaperRolls removeList a

main :: IO ()
main = do
  contents <- lines <$> readFile "day4/input.txt"
  let inputs = map (parse parser "") contents
  -- print inputs
  -- print contents
  validInput <- validateParse inputs

  let mapInput = toMatrix validInput

  case mapInput of
    Just m -> do
      let b = map (`countNeighbors` m) (indices m)

      putStrLn "Result part 1:"
      print (sum $ map (\x -> if x < 4 then 1 else 0) b)

      let (_, res2) = removeAllPossiblePaperRolls m
      putStrLn "Result part 2:"
      print res2
    _ -> return ()

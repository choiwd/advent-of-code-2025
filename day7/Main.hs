module Main where

import Data.Array.IArray
  ( Array,
    assocs,
    bounds,
    elems,
    ixmap,
    listArray,
    (!),
    (//),
  )
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.List.HT (allEqual, sliceVertical)
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
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

data MapPosition = Start | Empty | Splitter deriving (Eq)

instance Show MapPosition where
  show :: MapPosition -> String
  show Start = "S"
  show Empty = "."
  show Splitter = "^"

mapPosition :: Parser MapPosition
mapPosition = try (char 'S' $> Start) <|> try (char '.' $> Empty) <|> (char '^' $> Splitter)

parser :: Parser [MapPosition]
parser = some mapPosition <* eof -- Force consumption till end

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
        then pure . listArray ((0, 0), (length l - 1, length x - 1)) $ concat l
        else Nothing
    [] -> Nothing

prettyPrint :: (Show a) => Array (Int, Int) a -> IO ()
prettyPrint a = do
  let ((_, i), (_, i2)) = bounds a
      e = sliceVertical (i2 - i + 1) (elems a)
  mapM_ print e

prettyPrint' :: (Show a) => Maybe (Array (Int, Int) a) -> IO ()
prettyPrint' x =
  case x of
    Just a -> prettyPrint a
    _ -> print "Nothing"

irradiate :: (Array Int Int, Int) -> Array Int MapPosition -> (Array Int Int, Int)
irradiate (currentFrame, nBeams) env = foldl' f (currentFrame, nBeams) (assocs env)
  where
    f (frame, beamCounter) (i, mapType)
      | n_rays > 0 && mapType == Splitter =
          (frame // [update (i - 1), (i, 0), update (i + 1)], beamCounter + 1)
      | mapType == Start = (frame // [(i, 1)], beamCounter)
      | otherwise = (frame, beamCounter)
      where
        update pos = (pos, frame ! pos + n_rays)
        n_rays = frame ! i

run :: Array (Int, Int) MapPosition -> (Array Int Int, Int)
run inputMap = foldl' irradiate (initialFrame, 0) mapAsList
  where
    initialFrame = listArray (bimap snd snd $ bounds inputMap) [0, 0 ..]
    ((min_v, min_h), (max_v, max_h)) = bounds inputMap
    mapAsList = map (\y -> ixmap (min_h, max_h) (y,) inputMap) [min_v .. max_v]

main :: IO ()
main = do
  contents <- lines <$> readFile "day7/input.txt"
  let inputs = map (parse parser "") contents

  -- print contents
  validInput <- validateParse inputs

  let inputMap = toMatrix validInput
      x = run <$> inputMap
      res = snd <$> x
      res2 = sum . elems . fst <$> x

  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

module Main where

import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    parseMaybe,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (char, numberChar)

type Parser = Parsec Void String

data Rotation = ToLeft | ToRight deriving (Show)

integer :: Parser Int
integer = read <$> some numberChar

rotation :: Parser Rotation
rotation = try (isLeft <|> isRight)
  where
    isLeft = char 'L' *> return ToLeft
    isRight = char 'R' *> return ToRight

parser :: Parser (Rotation, Int)
parser = (,) <$> rotation <*> integer

main :: IO ()
main = do
  contents <- lines <$> readFile "day1/input.txt"
  let inputs = mapMaybe (parseMaybe parser) contents
      start :: (Int, Int) = (50, 0)
      res = foldl' f start inputs
      f (pos, count) (r, i) = (sum_, countNew)
        where
          countNew = if sum_ == 0 then count + 1 else count
          sum_ = case r of
            ToLeft -> mod (pos - i) 100
            ToRight -> mod (pos + i) 100

      res2 = foldl' f2 start inputs
      f2 (pos, count) (r, i) =
        let g pos_ delta
              | delta > pos_ && pos /= 0 = div (100 + (delta - pos)) 100
              | delta > pos_ = div (delta - pos) 100
              | delta == pos_ = 1
              | otherwise = 0
         in case r of
              ToLeft -> (mod (pos - i) 100, count + (g pos i))
              ToRight -> (mod (pos + i) 100, count + (div (pos + i) 100))

  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

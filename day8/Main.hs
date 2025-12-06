module Main where

import Control.Applicative.HT (lift2)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor (($>))
import Data.List (sortOn, (!?))
import Data.Ord (Down (Down))
import Data.Set (Set, fromList, size, union)
import Data.Void (Void)
import GHC.Float (int2Float)
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

data JunctionBox = Box Int Int Int deriving (Eq, Show)

data Connection = Conn Int Int Float deriving (Eq)

type Circuit = Set Int -- No Foldable instance for IntSets :<

parser :: Parser JunctionBox
parser = Box <$> num <*> num <*> num -- Force consumption till end
  where
    num = read <$> some digitChar <* commaOrEnd
    commaOrEnd = try (char ',' $> ()) <|> eof

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

distanceList :: [JunctionBox] -> [Connection]
distanceList m = [Conn x y (distance a b) | (x, a) <- e, (y, b) <- e, x < y]
  where
    distance (Box a b c) (Box x y z) = sqrt (sq x a + sq y b + sq z c)
    sq i j = int2Float ((i - j) ^ 2)
    e = zip [0 ..] m

getCircuits :: Connection -> [Circuit] -> (Maybe Circuit, Maybe Circuit, [Circuit])
getCircuits (Conn a b _) = foldr f (Nothing, Nothing, [])
  where
    f x (ca, cb, l)
      | a `elem` x = (Just x, cb, l)
      | b `elem` x = (ca, Just x, l)
      | otherwise = (ca, cb, x : l)

connectTo :: Connection -> [Circuit] -> [Circuit]
connectTo connection circuits =
  case (union <$> c1 <*> c2) <|> c1 <|> c2 of
    Just x -> union x newConnections : l
    _ -> newConnections : circuits
  where
    (c1, c2, l) = getCircuits connection circuits
    Conn a b _ = connection
    newConnections = fromList [a, b]

-- Connections must be sorted by distance
makeNCircuits :: Int -> [Connection] -> ([Circuit], [Connection], Maybe Connection)
makeNCircuits n_ c = go n_ ([], c, Nothing)
  where
    go n (circuits, x : xs, lastConn)
      | n > 0 && firstCircuitSize circuits < size nodes =
          go (n - 1) (x `connectTo` circuits, xs, Just x)
      | otherwise = (circuits, x : xs, lastConn)
    go _ (circuits, [], lastConn) = (circuits, [], lastConn)
    nodes = fromList $ concat [[i, j] | (Conn i j _) <- c]
    firstCircuitSize (a : _) = size a
    firstCircuitSize _ = 0

main :: IO ()
main = do
  contents <- lines <$> readFile "day8/input.txt"
  let inputs = map (parse parser "") contents

  validInput <- validateParse inputs

  let connections = sortOn (\(Conn _ _ x) -> x) $ distanceList validInput
      c = sortOn Down . map length . (\(a, _, _) -> a) $ makeNCircuits 1000 connections
      res = product (take 3 c)

      d = (\(_, _, a) -> a) $ makeNCircuits 1000000 connections
      getIndices (Conn a b _) = (a, b)
      getX (Box x _ _) = x
      res2 =
        (\(x1, x2) -> lift2 (*) (getX <$> x1) (getX <$> x2))
          . bimap (validInput !?) (validInput !?)
          . getIndices
          =<< d

  putStrLn "Result part 1:"
  print res

  putStrLn "Result part 2:"
  print res2

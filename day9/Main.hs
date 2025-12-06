module Main where

import Data.Function (on)
import Data.Functor (($>))
import Data.List (foldl1', maximumBy, tails)
import Data.List.HT (allEqual, mapAdjacent, rotate)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Debug.Trace (traceShow)
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

data Tile = Tile Int Int deriving (Eq, Show)

data Rectangle = Rectangle Tile Tile deriving (Eq, Show)

data Direction = North | South | East | West deriving (Eq, Show)

instance Num Tile where
  (+) :: Tile -> Tile -> Tile
  (+) (Tile a b) (Tile c d) = Tile (a + c) (b + d)
  negate :: Tile -> Tile
  negate (Tile a b) = Tile (-a) (-b)

parser :: Parser Tile
parser = Tile <$> num <*> num <* eof -- Force consumption till end
  where
    num = read <$> some digitChar <* commaOrEnd
    commaOrEnd = try (char ',' $> ()) <|> eof

sizeR :: Rectangle -> Int
sizeR (Rectangle (Tile a b) (Tile c d)) = (abs (c - a) + 1) * (abs (d - b) + 1)

traceIf :: (Show a) => Bool -> a -> b -> b
traceIf cond a b = if cond then traceShow a b else b

validateParse :: [Either ParseError b] -> IO [b]
validateParse (x : xs) =
  case x of
    Right i -> (i :) <$> validateParse xs
    Left bundle -> putStr (errorBundlePretty bundle) *> validateParse xs
validateParse [] = pure []

maxRectangle :: [Tile] -> Maybe (Rectangle, Int)
maxRectangle l = a
  where
    a = safeMax $ mapMaybe (safeMax . map (\i -> (i, sizeR i)) . mkRectangles) (tails l)
    mkRectangles (r : rs : rss) = Rectangle r rs : mkRectangles (r : rss)
    mkRectangles _ = []
    safeMax x
      | not $ null x = Just $ maximumBy (compare `on` snd) x
      | otherwise = Nothing

maxRectangle2 :: [Tile] -> Maybe (Rectangle, Int)
maxRectangle2 l_ =
  case l_ of
    x : xs -> pure $ foldl1' f a
      where
        a = concatMap (map (\i -> (i, sizeR i)) . mkRectangles) (tails l_)
        mkRectangles (r : rs : rss) = Rectangle r rs : mkRectangles (r : rss)
        mkRectangles _ = []
        f (r1, s1) (r2, s2)
          | s2 > s1 && validVertices r2 && noEdgesCrossing r2 = (r2, s2)
          | otherwise = (r1, s1)

        polygonEdges = mapAdjacent (,) (x : xs ++ [x])
        validVertices r = allVerticesInside (vertices r) polygonEdges
        vertices (Rectangle (Tile i j) (Tile k l)) = [Tile i j, Tile i l, Tile k l, Tile k j]
        noEdgesCrossing r = not . or $ map edgeIntersect (edges r) <*> polygonEdges
        edges r = mapAdjacent (,) (take 5 . cycle $ vertices r)
    _ -> Nothing

-- a b
-- c d
det2 :: (Num a) => a -> a -> a -> a -> a
det2 a b c d = (a * d) - (b * c)

rayCasting :: (Tile, Tile) -> (Tile, Tile) -> Bool
rayCasting (r1, r2) (r3, r4) =
  0 <= u && u <= 1 && 0 <= t
  where
    Tile a b = r1 - r2
    Tile c d = r3 - r4
    Tile e f = r1 - r3
    Tile g h = r3 - r4
    d1 = det2 a e b f
    d2 = det2 a c b d
    d3 = det2 e g f h
    u = (-int2Float d1) / int2Float d2
    t = int2Float d3 / int2Float d2

edgeIntersect :: (Tile, Tile) -> (Tile, Tile) -> Bool
edgeIntersect a b
  | any3Same [x1, x2, x3, x4] = False
  | any3Same [y1, y2, y3, y4] = False
  | otherwise = rayCasting a b && rayCasting b a
  where
    any3Same l = any (allEqual . take 3 . (`rotate` l)) [0 .. 3]
    ((r1, r2), (r3, r4)) = (a, b)
    ((Tile x1 y1, Tile x2 y2), (Tile x3 y3, Tile x4 y4)) = ((r1, r2), (r3, r4))

-- Considers only the last (rightmost) edge
validCrossing :: Tile -> (Tile, Tile, Tile, Tile) -> Bool
validCrossing p@(Tile _ yp) (p1, p2@(Tile _ yp2), p3@(Tile _ yp3), p4)
  | horizontalCrossing p (p3, p4) && isLeft p (p3, p4) && not isTangent = True
  | otherwise = False
  where
    isTangent = yp2 == yp3 && yp == yp2 && ((isUpward p1 p2 && isDownward p3 p4) || (isDownward p1 p2 && isUpward p3 p4))
    isUpward (Tile _ a) (Tile _ b) = a < b
    isDownward a b = isUpward b a
    isLeft (Tile px _) (Tile x0 _, Tile x1 _) = px < x0 && px < x1
    horizontalCrossing (Tile _ y) (a@(Tile _ y1), b@(Tile _ y2))
      | isDownward a b = y2 < y && y <= y1
      | isUpward a b = y1 <= y && y < y2
      | otherwise = False -- horizontal edge

-- For each edge, prepend the 2 points that creates its 3 preceeding edges
as4PointSequence :: [(Tile, Tile)] -> [(Tile, Tile, Tile, Tile)]
as4PointSequence l@(_ : _ : _ : _) = zipWith (\(a, b) (c, d) -> (a, b, c, d)) (rotate (-2) l) l
as4PointSequence _ = []

-- if even (out), and if odd (in)
pointInPolygon :: Tile -> [(Tile, Tile)] -> Bool
pointInPolygon p = isOdd . foldl' f 0 . as4PointSequence
  where
    isOdd = (/= 0) . (`mod` 2)
    f acc fourPoints
      | validCrossing p fourPoints = acc + 1
      | otherwise = acc

allVerticesInside :: [Tile] -> [(Tile, Tile)] -> Bool
allVerticesInside vertices edges = all f vertices
  where
    isPerimeter (Tile a b) =
      any
        ( \(Tile i j, Tile k l) ->
            (a == i && a == k && min j l <= b && b <= max j l) || (b == j && b == l && min i k <= a && a <= max i k)
        )
        edges
    isInside x = pointInPolygon x edges
    f x = isPerimeter x || isInside x

main :: IO ()
main = do
  contents <- lines <$> readFile "day9/input.txt"
  let inputs = map (parse parser "") contents

  validInput <- validateParse inputs

  let res = maxRectangle validInput
      res2 = maxRectangle2 validInput

  -- print validInput
  putStrLn "Result part 1:"
  print res
  putStrLn "Result part 2:"
  print res2

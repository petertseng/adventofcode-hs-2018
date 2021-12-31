{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)

import Data.List (dropWhileEnd, foldl', partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

closest :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
closest [] _ = Nothing
closest (x:xs) pt = fst (foldl' closer (Just x, dist pt x) xs)
  where closer (_, d) y | dist pt y == d = (Nothing, d)
        closer (c, d) y | dist pt y > d = (c, d)
        closer (_, _) y = (Just y, dist pt y)

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

freqMap :: Ord a => [a] -> Map a Int
freqMap = Map.fromListWith (+) . map (, 1)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

coord :: String -> (Int, Int)
coord s = case words s of
  [a, b] -> (read (dropWhileEnd (== ',') a), read b)
  _ -> error ("bad coord " ++ s)

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let coords = map coord (lines s)
      (xs, ys) = unzip coords
      (xmin, xmax) = (minimum xs, maximum xs)
      (ymin, ymax) = (minimum ys, maximum ys)
      pts = [(x, y) | x <- [xmin .. xmax], y <- [ymin .. ymax]]
      (border, internal) = partition (\(x, y) -> x == xmin || x == xmax || y == ymin || y == ymax) pts
      -- should I nubOrd infs if it would make the delete take less time?
      -- I don't know that it'll make much of a difference;
      -- nubOrd would insert into a set, delete deletes from a tree.
      infs = mapMaybe (closest coords) border
      sizes = freqMap (mapMaybe (closest coords) internal)
      finiteSizes = foldl' (flip Map.delete) sizes infs
  -- could use the faster implementation where you expand from each point,
  -- but it doesn't seem worth it.
  print (maximum (Map.elems finiteSizes))

  let maxDist = maybe 10000 read (lookup 'd' flags)
  print (count (\p -> sum (map (dist p) coords) < maxDist) pts)

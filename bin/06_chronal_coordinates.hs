{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)

import Data.Array.Unboxed (UArray, accumArray, elems)
import Data.List (dropWhileEnd, foldl', partition)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)

closest :: [Pos] -> Pos -> Maybe Pos
closest [] _ = Nothing
closest (x:xs) pt = fst (foldl' closer (Just x, dist pt x) xs)
  where closer (_, d) y | dist pt y == d = (Nothing, d)
        closer (c, d) y | dist pt y > d = (c, d)
        closer (_, _) y = (Just y, dist pt y)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

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
      -- considering nubOrd infs, but I expect it to take O(n log n) time,
      -- whereas just dealing with them normally should take O(n) time.
      infs = mapMaybe (closest coords) border
      sizes = mapMaybe (closest coords) internal
      finiteSizes = accumArray (\a b -> if a < 0 then a else a + b) 0 ((xmin, ymin), (xmax, ymax)) (map (, -1) infs ++ map (, 1) sizes) :: UArray Pos Int
  -- could use the faster implementation where you expand from each point,
  -- but it doesn't seem worth it.
  print (maximum (elems finiteSizes))

  let maxDist = maybe 10000 read (lookup 'd' flags)
  print (count (\p -> sum (map (dist p) coords) < maxDist) pts)

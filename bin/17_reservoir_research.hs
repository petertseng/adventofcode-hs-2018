{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)
data Ground = Clay | Flow | Still deriving (Eq, Show)

down :: Int -> Int -> Int -> Map Pos Ground -> Map Pos Ground
down maxY y x m | y == maxY = Map.insert (y, x) Flow m
down maxY y _ _ | y > maxY = error "went too far"
down maxY y x m = case Map.lookup (y + 1, x) m of
  Nothing -> down maxY (y + 1) x (Map.insert (y, x) Flow m)
  Just Flow -> Map.insert (y, x) Flow m
  Just Still -> leftRight maxY y x m
  Just Clay -> leftRight maxY y x m

leftRight :: Int -> Int -> Int -> Map Pos Ground -> Map Pos Ground
leftRight maxY y x m = case (scanX m y x (-1), scanX m y x 1) of
  (Right l, Right r) -> leftRight maxY (y - 1) x (updateXs y [l .. r] Still m)
  (Left l, Right r) -> down maxY y l (updateXs y [l .. r] Flow m)
  (Right l, Left r) -> down maxY y r (updateXs y [l .. r] Flow m)
  (Left l, Left r) -> down maxY y l (down maxY y r (updateXs y [l .. r] Flow m))

-- left X: drop at X, right: wall after X
-- (in both cases, X is the last accessible X)
scanX :: Map Pos Ground -> Int -> Int -> Int -> Either Int Int
scanX m y x dx = case (Map.lookup (y + 1, x) m, Map.lookup (y, x + dx) m) of
  (Just Flow, _) -> Left x
  (Nothing, _) -> Left x
  (_, Just Clay) -> Right x
  (_, _) -> scanX m y (x + dx) dx

updateXs :: Int -> [Int] -> Ground -> Map Pos Ground -> Map Pos Ground
updateXs y xs g m = Map.unionWith (\_ x -> x) m (Map.fromList [((y, x), g) | x <- xs])

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

vein :: String -> [Pos]
vein s = case words s of
  ['x':'=':x, 'y':'=':ys] -> map (, read (dropWhileEnd (== ',') x)) (range ys)
  ['y':'=':y, 'x':'=':xs] -> map (read (dropWhileEnd (== ',') y), ) (range xs)
  _ -> error ("bad vein " ++ s)

range :: String -> [Int]
range s = case splitOn '.' s of
  [l, r] -> [read l .. read r]
  _ -> error ("bad range " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let clay = map vein (lines s)
      ys = map fst (concat clay)
      maxY = maximum ys
      minY = minimum ys
      ground0 = Map.fromList (map (, Clay) (concat clay))
      ground = down maxY minY 500 ground0
      flows = count (== Flow) (Map.elems ground)
      stills = count (== Still) (Map.elems ground)
  print (flows + stills)
  print stills

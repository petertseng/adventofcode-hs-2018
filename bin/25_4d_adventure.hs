{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.List (tails)
import Data.Traversable (for)

type Point = (Int, Int, Int, Int)

numGroups :: (Int, Int) -> [(Int, Int)] -> Int
numGroups bounds@(minId, maxId) pairs = runST $ do
  parent <- newListArray bounds [minId .. maxId] :: ST s (STUArray s Int Int)
  rank   <- newArray bounds 0 :: ST s (STUArray s Int Int)

  let find x = do
        parX <- readArray parent x
        if x /= parX
          then do
            parParX <- find parX
            writeArray parent x parParX
            return parParX
          else return x

  let union x y = do
        parX <- find x
        parY <- find y
        when (parX /= parY) $ do
          rankX <- readArray rank parX
          rankY <- readArray rank parY
          case compare rankX rankY of
            LT -> writeArray parent parX parY
            GT -> writeArray parent parY parX
            EQ -> do
              writeArray parent parY parX
              writeArray rank parX (rankX + 1)
        return (parX /= parY)

  successfulUnions <- for pairs (uncurry union)
  return (maxId - minId + 1 - length (filter id successfulUnions))

close :: ((Int, Point), (Int, Point)) -> Bool
close ((_, (w1, x1, y1, z1)), (_, (w2, x2, y2, z2))) = sum (map abs [w1 - w2, x1 - x2, y1 - y2, z1 - z2]) <= 3

point :: String -> Point
point s = case map read (splitOn ',' s) of
  [a, b, c, d] -> (a, b, c, d)
  _ -> error ("bad point " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let points = map point (lines s)
      pairs = [(x, y) | x:ys <- tails (zip [0..] points), y <- ys]
      goodPairs = filter close pairs
  print (numGroups (0, length points - 1) (map (\((a, _), (b, _)) -> (a, b)) goodPairs))

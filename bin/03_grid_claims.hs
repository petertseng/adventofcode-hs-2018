import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***), second)
import Control.Monad (foldM)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Foldable (for_)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
--import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
--import Data.List (dropWhileEnd, foldl')
import Data.List (dropWhileEnd)
--import Data.Maybe (isNothing)

-- MArray faster here: 0.8 seconds with IntMap. 0.3 with MArray

countClash :: [Int] -> [(Int, Int)] -> (Int, [Int])

{-
-- IntMap, with counter
countClash ids claims = (clashed, IntSet.toList unclashed)
  where (_, unclashed, clashed) = foldl' addOrClash (IntMap.empty, IntSet.fromList ids, 0) claims

-- IntMap, without counter
--countClash ids claims = (count isNothing (IntMap.elems result), IntSet.toList unclashed)
  --where (result, unclashed, _) = foldl' addOrClash (IntMap.empty, IntSet.fromList ids, 0) claims

-- Just n: Claimed by exactly one
-- Nothing: Claimed by too many
 adding the int counter instead of `count isNothing (IntMap.elems result)` doesn't help speed it up
addOrClash :: (IntMap (Maybe Int), IntSet, Int) -> (Int, Int) -> (IntMap (Maybe Int), IntSet, Int)
addOrClash (claims, unclashed, clashed) (i, pos) = case IntMap.lookup pos claims of
  Just (Just prev) | prev == i -> (claims, unclashed, clashed)
  Just (Just prev) -> (IntMap.insert pos Nothing claims, IntSet.delete prev (IntSet.delete i unclashed), clashed + 1)
  Just Nothing -> (claims, IntSet.delete i unclashed, clashed)
  Nothing -> (IntMap.insert pos (Just i) claims, unclashed, clashed)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
-}

-- MArray
-- -2: Claimed by too many
-- -1: Unclaimed
countClash ids claims = second IntSet.toList $ runST $ do
  grid <- newArray (0, 999999) (-1) :: ST s (STUArray s Int Int)
  foldM (\(clashed, unclashed) (i, pos) -> do
      prev <- readArray grid pos
      case prev of
        p | p == i -> return (clashed, unclashed)
        -1 -> writeArray grid pos i >> return (clashed, unclashed)
        -2 -> return (clashed, IntSet.delete i unclashed)
        p -> writeArray grid pos (-2) >> return (clashed + 1, IntSet.delete p (IntSet.delete i unclashed))
    ) (0, IntSet.fromList ids) claims

expand :: (Int, (Int, Int), (Int, Int)) -> [(Int, Int)]
expand (i, (y0, x0), (h, w)) = [(i, y * 1000 + x) | y <- [y0 .. y0 + h - 1], x <- [x0 .. x0 + w - 1]]

claim :: String -> (Int, (Int, Int), (Int, Int))
claim s = case words s of
  ['#':i, "@", pos, size] -> (read i, readPos pos, readSize size)
    where readPos = (read *** read) . splitOnOne ',' . dropWhileEnd (== ':')
          readSize = (read *** read) . splitOnOne 'x'
  _ -> error ("bad claim " ++ s)


main :: IO ()
main = do
  s <- readInputFile
  let claims = map claim (lines s)
      ids = map (\(i, _, _) -> i) claims
      (clashed, unclashed) = countClash ids (concatMap expand claims)
  print clashed
  for_ unclashed print

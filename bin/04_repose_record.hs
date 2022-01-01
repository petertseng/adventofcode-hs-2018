{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy, sort)
import Data.Ord (comparing)

data Event = Guard Int | Sleep Int | Wake Int deriving Show

guardBy :: ([Int] -> Int) -> IntMap (IntMap Int) -> Int
guardBy f = uncurry (*) . second bestMinute . bestGuard
  where bestGuard :: IntMap (IntMap Int) -> (Int, IntMap Int)
        bestGuard = maximumBy (comparing (f . IntMap.elems . snd)) . IntMap.assocs
        bestMinute :: IntMap Int -> Int
        bestMinute = fst . maximumBy (comparing snd) . IntMap.assocs

-- Part of this could have been:
-- groupBy (\_ b -> (not . isGuard) b)
-- However, I decided this was dangerous.
-- groupBy assumes its equality predicate is an equality.
-- However the provided function (\_ b -> f b)
-- would not be reflexive nor symmertric nor transitive.
-- So it happens to work now, but could be broken in the future,
-- as it breaks the contract of groupBy.
splitGuard :: [Event] -> [(Int, IntMap Int)]
splitGuard [] = []
splitGuard (Guard g:xs) = (g, freqMap (guard gs)) : splitGuard others
  where (gs, others) = break isGuard xs
splitGuard xs = error ("bad guards " ++ show xs)

freqMap :: [Int] -> IntMap Int
freqMap = IntMap.fromListWith (+) . map (, 1)

guard :: [Event] -> [Int]
guard [] = []
guard (Guard _:_) = error "guard in guard"
guard (Wake _:_) = error "wake without being asleep"
guard (Sleep s:Wake w:xs) = [s .. (w - 1)] ++ guard xs
guard (Sleep _:Sleep _:_) = error "double sleep"
guard (Sleep _:Guard _:_) = error "guard in guard"
guard [Sleep _] = error "never woke up"

isGuard :: Event -> Bool
isGuard (Guard _) = True
isGuard (Sleep _) = False
isGuard (Wake _) = False

event :: String -> Event
event s = case words s of
  [_, _, "Guard", '#':n, "begins", "shift"] -> Guard (read n)
  [_, _:_:':':m1:m2:"]", "falls", "asleep"] -> Sleep (read [m1, m2])
  [_, _:_:':':m1:m2:"]", "wakes", "up"] -> Wake (read [m1, m2])
  _ -> error ("bad event " ++ s)

main :: IO ()
main = do
  s <- readInputFile
  let events = map event (sort (lines s))
      guards = IntMap.fromListWith (IntMap.unionWith (+)) (splitGuard events)
  print (guardBy sum guards)
  -- guard who doesn't sleep -> 0
  print (guardBy (maximum . (0:)) guards)

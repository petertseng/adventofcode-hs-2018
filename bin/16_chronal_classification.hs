import AdventOfCode (readInputFile)
import AdventOfCode.Assembly (Op(..), Regs, exec, regsFromList)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Either (fromRight)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type UnknownInst = (Int, Int, Int, Int)
type Sample = (Regs, UnknownInst, Regs)

candidates :: Sample -> [Op]
candidates (before, (_, a, b, c), after) = filter (\op -> exec (op, a, b, c) before == after) [Addr .. Eqrr]

-- 2020 AdventOfCode.BipartiteMatching
-- except using IntMap instead of Map k
-- Left if we're uncertain, Right if we're certain
match :: Ord v => IntMap (Set v) -> IntMap (Either (Set v) v)
match m = untilEq nakedSingle (IntMap.map Left m)

nakedSingle :: Ord a => IntMap (Either (Set a) a) -> IntMap (Either (Set a) a)
nakedSingle ps = foldl' confirm ps (mapMaybe singleLeft (IntMap.assocs ps))
  where confirm p (l, r) = IntMap.insert l (Right r) (IntMap.map (mapLeft (Set.delete r)) p)
        singleLeft (l, Left rs) | Set.size rs == 1 = Just (l, Set.findMin rs)
        singleLeft (_, _) = Nothing

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

untilEq :: Eq a => (a -> a) -> a -> a
untilEq f = fst . until2 (==) f

until2 :: (a -> a -> Bool) -> (a -> a) -> a -> (a, a)
until2 p f x = let x' = f x in if p x x' then (x, x') else until2 p f x'

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

sample :: [String] -> Sample
sample [before, i, after] = (regs before, inst i, regs after)
  where regs = regsFromList . map (read . dropWhile (== ' ')) . splitOn ',' . between '[' ']'
sample _ = error "bad sample "

inst :: String -> UnknownInst
inst s = case map read (words s) of
  [a, b, c, d] -> (a, b, c, d)
  _ -> error "bad inst"

between :: Eq a => a -> a -> [a] -> [a]
between l r = fst . splitOnOne r . snd . splitOnOne l

isSample :: [String] -> Bool
isSample ['B':'e':'f':'o':'r':'e':_, _, 'A':'f':'t':'e':'r':_] = True
isSample _ = False

main :: IO ()
main = do
  s <- readInputFile
  let (samples, prog) = case span isSample (splitOn "" (lines s)) of
        (sam, [p]) -> (sam, p)
        (_, []) -> error "no program"
        (_, _:_:_) -> error "too many programs"
      cand sam@(_, (i, _, _, _), _) = (i, candidates sam)
      cands = map (cand . sample) samples
  print (count ((>= 3) . length . snd) cands)
  let possibility = IntMap.fromListWith Set.intersection [(i, Set.fromList ops) | (i, ops) <- cands]
      matches = match possibility
      confirmed = IntMap.map (fromRight (error "unknown")) matches
      decodedProg = map ((\(a, b, c, d) -> (confirmed IntMap.! a, b, c, d)) .inst) prog
  print (foldl' (flip exec) (regsFromList [0, 0, 0, 0]) decodedProg IntMap.! 0)

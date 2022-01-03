{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Control.Arrow ((&&&), first)
import Data.List (delete, dropWhileEnd, find, foldl', mapAccumR, maximumBy, partition, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Ord (comparing)

-- record? don't really care
-- I have decided that negative initiative should be stored first.
-- That will make higher initiative units naturally come first in sort order.
-- -initiative, hp, effectiveness, damage, damage type, isImmuneSys
type Group = (Int, Int, Map String Int, Int, String, Bool)

-- true: immune system won
-- false: infection won
-- Nothing: stalemate
combat :: Map Group Int -> Maybe (Bool, Int)
combat g = case fight g of
  g' | uniformBy isImmuneSys (Map.keys g') -> Just (isImmuneSys (head (Map.keys g')), sum (Map.elems g'))
  g' | g' == g -> Nothing
  g' -> combat g'

fight :: Map Group Int -> Map Group Int
fight grps = foldl' attack grps (Map.assocs targ)
  where (imm, inf) = partition (isImmuneSys . fst) (Map.assocs grps)
        targ = Map.union (selectTargets imm inf) (selectTargets inf imm)

attack :: Map Group Int -> (Group, Group) -> Map Group Int
attack grps (atk, def) = case Map.lookup atk grps of
  Nothing -> grps -- died earlier
  Just n -> Map.update (\n2 -> positive (n2 - kills n atk def)) def grps

selectTargets :: [(Group, Int)] -> [(Group, Int)] -> Map Group Group
selectTargets army enemy = Map.fromList (catMaybes (snd (mapAccumR select enemy (sortOn (effectivePower &&& initiative . fst) army))))
  where select [] _ = ([], Nothing)
        select remain (grp, _) =
          let (target, n) = maximumBy (comparing ((grp `effAgainst`) . fst &&& effectivePower &&& initiative . fst)) remain in
          if grp `effAgainst` target == 0 then (remain, Nothing) else (delete (target, n) remain, Just (grp, target))

kills :: Int -> Group -> Group -> Int
kills n atk def = effectivePower (atk, n) * (atk `effAgainst` def) `quot` hp def

effAgainst :: Group -> Group -> Int
(_, _, _, _, atkType, _) `effAgainst` (_, _, eff, _, _, _) = Map.findWithDefault 1 atkType eff

effectivePower :: (Group, Int) -> Int
effectivePower ((_, _, _, dmg, _, _), n) = dmg * n

hp :: Group -> Int
hp (_, h, _, _, _, _) = h

initiative :: Group -> Int
initiative (i, _, _, _, _, _) = -i

-- A little unfortunate that there's an immune system,
-- and that groups can be immune to damage types.
-- To avoid confusion resulting from isImmune, let's call it isImmuneSys
isImmuneSys :: Group -> Bool
isImmuneSys (_, _, _, _, _, b) = b

boost :: Int -> Group -> Group
boost bst (a, b, c, dmg, d, e) = (a, b, c, dmg + bst, d, e)

positive :: Int -> Maybe Int
positive n | n > 0 = Just n
positive _ = Nothing

uniformBy :: Eq b => (a -> b) -> [a] -> Bool
uniformBy _ [] = error "empty uniformBy"
uniformBy f (x:xs) = all ((== f x) . f) xs

groupAndNum :: Bool -> String -> (Group, Int)
groupAndNum b s = ((-ini, h, eff, dmg, dmgType, b), n)
  where ((l, r), eff) = case splitOnOne '(' s of
          (_, "") -> (splitAt 7 (words s), Map.empty)
          (l2, r2) -> let (l3, r3) = splitOnOne ')' r2 in ((words l2, words r3), effectiveness l3)
        (n, h) = case l of
          [nn, "units", "each", "with", hh, "hit", "points"] -> (read nn, read hh)
          _ -> error ("bad left " ++ show l)
        (dmg, dmgType, ini) = case r of
          ["with", "an", "attack", "that", "does", dn, dt, "damage", "at", "initiative", i] -> (read dn, dt, read i)
          _ -> error ("bad right " ++ show r)

effectiveness :: String -> Map String Int
effectiveness s = Map.fromList (concatMap eff (splitOn ';' s))
  where eff s' = case words s' of
          ("weak":"to":xs) -> pairEff 2 xs
          ("immune":"to":xs) -> pairEff 0 xs
          _ -> error ("bad " ++ s')
        pairEff n = map ((, n) . dropWhileEnd (== ','))

main :: IO ()
main = do
  s <- readInputFile
  let (immune, infect) = case splitOnOne "" (lines s) of
        ("Immune System:":imm, "Infection:":inf) -> (map (groupAndNum True) imm, map (groupAndNum False) inf)
        _ -> error "bad armies"
      grps = Map.fromList (immune ++ infect)
  print (snd (fromJust (combat grps)))

  let inf = Map.fromList infect
      boosted i = Map.union inf (Map.fromList (map (first (boost i)) immune))
      boosteds = [combat (boosted i) | i <- [1..]]
  print (snd (fromJust (fromJust (find (maybe False fst) boosteds))))

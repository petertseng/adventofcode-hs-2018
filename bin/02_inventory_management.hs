import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Data.Foldable (for_)
import Data.List (group, inits, mapAccumL, sort, tails)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

matchBox :: Set (String, String) -> String -> (Set (String, String), [String])
matchBox seen s = second catMaybes (mapAccumL matchParts seen (zip (inits s) (drop 1 (tails s))))

matchParts :: Set (String, String) -> (String, String) -> (Set (String, String), Maybe String)
matchParts seen (l, r) = (Set.insert (l, r) seen, if (l, r) `Set.member` seen then Just (l ++ r) else Nothing)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let boxes = lines s
      counts = map (map length . group . sort) boxes
      check n = count (n `elem`) counts
  print (check 2 * check 3)

  let (_, seens) = mapAccumL matchBox Set.empty boxes
  for_ (concat seens) putStrLn

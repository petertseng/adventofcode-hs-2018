import AdventOfCode (readInputFile)
import AdventOfCode.Search (bfs)

--import Control.Monad (foldM)
--import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (rights)
import Data.List (mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = (Int, Int)
-- Three types of positions: Original, Inactive, Active
-- Frames (storing original and inactive positions) are started by ( or ^.
-- When a frame is started, active positions become saved as original.
-- After that, original positions never change.
-- A frame starts out with no inactive positions.
-- On a |, active positions become inactive and are restored from original.
-- On a ), a frame ends and position becomes union of active and inactive positions.
-- Originals are no longer needed when a frame ends.
type Frame = (Char, [Pos], [Pos])
-- State: previous frames, active positions.
type State = ([Frame], [Pos])

-- This could potentially be a good use for Writer monad.
-- But it doesn't really make the code look too much better.
--walk2 :: State -> Char -> Writer [(Pos, Pos)] State
--wherever walk has (x, []), walk2 has return x
--wherever walk calls step, walk2 instead calls step2
--step2 :: State -> Int -> Int -> Writer [(Pos, Pos)] State
--step2 (f, poses) dy dx = tell (zip poses poses') >> return (f, poses')

walk :: State -> Char -> (State, [(Pos, Pos)])
walk ([], poses) '^' = (([newFrame '^' poses], poses), [])
walk _ '^' = error "illegal start"
walk ([('^', _, _)], _) '$' = (([newFrame '$' []], []), [])
walk _ '$' = error "illegal end"
walk (fs, poses) '(' = ((newFrame '(' poses : fs, poses), [])
walk ((c, orig, inactive):fs, active) '|' = (((c, orig, inactive ++ active) : fs, orig), [])
walk ([], _) '|' = error "alternative without frame"
walk (('(', _, inactive):fs, active) ')' = ((fs, nubOrd (inactive ++ active)), [])
walk ([], _) ')' = error "close paren without frame"
walk s 'E' = step s 0 1
walk s 'W' = step s 0 (-1)
walk s 'N' = step s 1 0
walk s 'S' = step s (-1) 0
walk ([('$', _, _)], _) '\n' = (error "walking after newline", [])
walk (_, _) '\n' = error "unexpected newline"
walk _ c = error (c : " bad walk")

step :: State -> Int -> Int -> (State, [(Pos, Pos)])
step (f, poses) dy dx = ((f, poses'), zip poses poses')
  where poses' = [(y + dy, x + dx) | (y, x) <- poses]

newFrame :: Char -> [Pos] -> Frame
newFrame c poses = (c, poses, [])

bidirMap :: Ord a => [(a, a)] -> Map a [a]
bidirMap = Map.fromListWith (++) . concatMap pair
  where pair (a, b) = [(a, [b]), (b, [a])]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let (_, conns) = mapAccumL walk ([], [(0, 0)]) s
      m = bidirMap (concat conns)
      --conns2 = execWriter (foldM walk2 ([], [(0, 0)]) s)
      --m = bidirMap (concat conns2)
      dists = map fst (rights (bfs (m Map.!) (const True) (0, 0)))
  print (maximum dists)
  print (count (> 999) dists)

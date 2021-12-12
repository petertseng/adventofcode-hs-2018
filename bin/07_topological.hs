import AdventOfCode (readInputFileAndFlags)

import Data.Char (ord)
import Data.List (delete, foldl', mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

-- deps: K task with dependencies, V task that must be finished before K
-- free: tasks that currently can be started (all dependencies finished)
task :: Ord a => Map a [a] -> Set a -> [a]
task deps free | Map.null deps = Set.toList free
task deps free = t : task deps' free''
  where (deps', free'') = finishTask (deps, free') t
        -- if free is empty, no tasks to give out, let it error.
        (t, free') = Set.deleteFindMin free

finishTask :: Ord a => (Map a [a], Set a) -> a -> (Map a [a], Set a)
finishTask (deps, free) t = (Map.filter (not . null) deps', free `Set.union` Set.fromList newFree)
  where (newFree, deps') = Map.mapAccumWithKey removeDep [] deps
        removeDep acc k [t'] | t' == t = (k : acc, [])
        removeDep acc _ v = (acc, delete t v)

-- Just: (time left, task)
-- Nothing:  not working on a task
type Runner a = Maybe (Int, a)

parTask :: Ord a => Int -> (a -> Int) -> Map a [a] -> Set a -> Int
parTask nrunners time deps free = parTask' time runners deps free'
  where (free', runners) = assign time free (replicate nrunners Nothing)

parTask' :: Ord a => (a -> Int) -> [Runner a] -> Map a [a] -> Set a -> Int
-- all tasks assigned; let runners finish
parTask' _ runners deps free | Map.null deps && Set.null free = maximum (map fst (catMaybes runners))
parTask' time runners deps free = mt + parTask' time runners'' deps' free''
  where mt = minTime runners
        (finish, runners') = progressRunners mt runners
        (deps', free') = foldl' finishTask (deps, free) finish
        (free'', runners'') = assign time free' runners'

minTime :: [Maybe (Int, a)] -> Int
minTime = minimum . map fst . catMaybes

assign :: (a -> Int) -> Set a -> [Runner a] -> (Set a, [Runner a])
assign time = mapAccumL assign1
  where assign1 free Nothing = case Set.minView free of
          Nothing -> (free, Nothing) -- no task to give out
          Just (t, free') -> (free', Just (time t, t))
        assign1 free j = (free, j)

progressRunners :: Int -> [Runner a] -> ([a], [Runner a])
progressRunners t = mapAccumL progress1 []
  where progress1 acc Nothing = (acc, Nothing)
        progress1 _ (Just (t', _)) | t' < t = error "advanced too far"
        progress1 acc (Just (t', tsk)) | t' == t = (tsk : acc, Nothing)
        progress1 acc (Just (t', tsk)) = (acc, Just (t' - t, tsk))

dep :: String -> (Char, Char)
dep s = case words s of
  ["Step", [a], "must", "be" , "finished", "before", "step", [b], "can", "begin."] -> (a, b)
  _ -> error ("bad dep: " ++ s)

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let revDeps = map dep (lines s)
  let deps = Map.fromListWith (++) [(b, [a]) | (a, b) <- revDeps]
      free = Set.fromList (map fst revDeps) `Set.difference` Map.keysSet deps
  putStrLn (task deps free)
  let timeDelay = maybe 60 read (lookup 't' flags)
      nWorkers = maybe 5 read (lookup 'n' flags)
  print (parTask nWorkers (\l -> ord l - ord 'A' + 1 + timeDelay) deps free)

import Control.Monad (foldM, foldM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, modifyArray, newArray, readArray, writeArray)
import Data.Array.ST (STUArray)
--import Data.IntMap (IntMap)
--import qualified Data.IntMap as IntMap
--import Data.List (foldl')
--import Data.Sequence (Seq)
--import qualified Data.Sequence as Seq
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

marblesPerCycle :: Int
marblesPerCycle = 23

game :: Int -> Int -> Int
game players marbles = runST $ do
  right <- newArray (0, addingCycles marbles * marblesPerCycle - 1) 0 :: ST s (STUArray s Int Int)
  score <- newArray (0, players - 1) 0 :: ST s (STUArray s Int Int)
  writeArray right 0 0
  current <- foldM (\current cyc -> do
    let base = marblesPerCycle * cyc

    -- add marbles
    foldM_ (\cur i -> do
      let marble = base + i
      currentRight <- readArray right cur
      readArray right currentRight >>= writeArray right marble
      writeArray right currentRight marble
      return marble
      ) current [1 .. marblesPerCycle - 1]

    -- remove marble
    let marble = base + marblesPerCycle
    removed <- readArray right (marble - 5)
    modifyArray score (marble `rem` players) (+ (marble + removed))
    newCur <- readArray right removed
    writeArray right (marble - 5) newCur
    return newCur
    ) 0 [0 .. addingCycles marbles - 1]

  -- remove marbles only
  let nonAddingCycles = (marbles `quot` marblesPerCycle) - addingCycles marbles
  removed <- foldM (\r _ -> readArray right r) current [(), (), ()]

  foldM_ (\r cyc -> do
    removed' <- foldM (\r' _ -> readArray right r') r (replicate 16 ())
    let marble = marblesPerCycle * (cyc + addingCycles marbles)
    modifyArray score (marble `rem` players) (+ (marble + removed'))
    return removed'
    ) removed [1 .. nonAddingCycles]

  scores <- getElems score
  return (maximum scores)

-- not a tight bound, but don't feel like being precise
addingCycles :: Int -> Int
addingCycles marbles = (cycles + 1) `quot` 2
  where cycles = marbles `quot` marblesPerCycle

{-
game :: Int -> Int -> Int
game players marbles = maximum (IntMap.elems scores)
  where (scores, _) = foldl' (step players) (IntMap.empty, zempty) [1 .. marbles]

step :: Int -> (IntMap Int, Zipper Int) -> Int -> (IntMap Int, Zipper Int)
step players (scores, z) i | i `rem` marblesPerCycle == 0 = (IntMap.insertWith (+) (i `rem` players) (i + removed) scores, z')
  where (removed, z') = remove6L z
step _ (scores, z) i = (scores, insert1R i z)

-- seq-based zipper a little faster (7 seconds), but still quite bad
-- left end is elements to the left, since splitAt 6 will be used
type Zipper a = (a, Seq a)

zempty :: Zipper Int
zempty = (0, Seq.empty)

insert1R :: a -> Zipper a -> Zipper a
insert1R i (v, Seq.Empty) = (i, Seq.singleton v)
insert1R i (v, rs Seq.:|> r) = (i, r Seq.<| v Seq.<| rs)

remove6L :: Zipper a -> (a, Zipper a)
remove6L (v, z) = (remove, (newV, newL Seq.>< (v Seq.<| newR)))
  where (newR Seq.:|> newV, remove Seq.:<| newL) = Seq.splitAt 6 z
-}

{- list-based zipper correct but quite slow (10+ seconds), due to list reversing.
type Zipper a = ([a], a, [a])

zempty :: Zipper Int
zempty = ([], 0, [])

left :: Zipper a -> Zipper a
left z@([], _, []) = z
left ([], v, rs) = left (reverse rs, v, [])
left (l:ls, v, rs) = (ls, l, v:rs)

right :: Zipper a -> Zipper a
right z@([], _, []) = z
right (ls, v, []) = right ([], v, reverse ls)
right (ls, v, r:rs) = (v:ls, r, rs)

insertR :: a -> Zipper a -> Zipper a
insertR i (ls, v, rs) = (v:ls, i, rs)

insert1R :: a -> Zipper a -> Zipper a
insert1R i = insertR i . right

removeL :: Zipper a -> (a, Zipper a)
removeL ([], _, []) = error "nothing left"
removeL ([], v, rs) = removeL (reverse rs, v, [])
removeL (l:ls, v, rs) = (l, (ls, v, rs))

remove6L :: Zipper a -> (a, Zipper a)
remove6L z = removeL (iterate left z !! 6)
-}

findNumbers :: String -> (Int, Int)
findNumbers s = case mapMaybe readMaybe (words s) of
  [a, b] -> (a, b)
  _ -> error ("not two numbers in " ++ s)

main :: IO ()
main = do
  args <- getArgs
  (players, marbles) <- case args of
    a:b:_ -> return (read a, read b)
    f:_ -> fmap findNumbers (readFile f)
    _ -> fmap findNumbers (readFile "/dev/stdin")
  --print (foldl' (step 5) (IntMap.empty, zempty) [1 .. marblesPerCycle - 1])
  --print (foldl' (step 5) (IntMap.empty, zempty) [1 .. marblesPerCycle])
  --print (game 5 25)
  print (game players marbles)
  print (game players (marbles * 100))

import AdventOfCode.Search (astarInt)

import Control.Monad (when)
import Data.Array.Unboxed ((!), Array, UArray, array)
import Data.Maybe (fromJust, mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

type Pos = (Int, Int)
type PosAndTool = ((Int, Int), Int)

torch :: Int
torch = 1

toolChangeTime :: Int
toolChangeTime = 7

moveTime :: Int
moveTime = 1

terrains :: Int -> Int -> Int -> UArray Pos Int
-- can't fmap this one since this one's a UArray not an Array
terrains depth targy targx = array bds [((y, x), (ero ! (y, x)) `rem` 3) | y <- [0 .. height], x <- [0 .. width]]
  where bds = ((0, 0), (height, width))
        ero = fmap (\g -> (g + depth) `rem` 20183) geo
        -- Not actually sure this is far enough, but works okay so far
        width = targy + targx
        height = targy + targx
        geo = array bds (zz : zt : zx ++ zy ++ rest) :: Array (Int, Int) Int
        zz = ((0, 0), 0)
        zt = ((targy, targx), 0)
        zy = [((y, 0), y * 48271) | y <- [1 .. height], (y, 0) /= (targy, targx)]
        zx = [((0, x), x * 16807) | x <- [1 .. width], (0, x) /= (targy, targx)]
        rest = [((y, x), ero ! (y - 1, x) * ero ! (y, x - 1)) | y <- [1 .. height], x <- [1 .. width], (y, x) /= (targy, targx)]

neigh :: UArray Pos Int -> PosAndTool -> [(Int, PosAndTool)]
neigh terr ((y, x), tool) = switchTool : filter (\v -> inBounds v && toolOK v) cands
  where switchTool = (toolChangeTime, ((y, x), 3 - tool - terr ! (y, x)))
        cands = [(moveTime, ((y + dy, x + dx), tool)) | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]
        inBounds (_, ((ny, nx), _)) = ny >= 0 && nx >= 0
        toolOK (_, ((ny, nx), t)) = t /= terr ! (ny, nx)

compress :: Int -> PosAndTool -> Int
compress width ((y, x), t) = (y * width + x) * 4 + t

heur :: Int -> Int -> PosAndTool -> Int
heur targy targx ((y, x), t) = abs (y - targy) + abs (x - targx) + if t == torch then 0 else toolChangeTime

findNumbers :: String -> (Int, Int, Int)
findNumbers s = case mapMaybe readMaybe (words s) of
  [a, b, c] -> (a, b, c)
  _ -> error ("not three numbers in " ++ s)

main :: IO ()
main = do
  args <- getArgs
  (depth, targx, targy) <- case args of
    a:b:c:_ -> return (read a, read b, read c)
    f:_ -> fmap findNumbers (readFile f)
    _ -> fmap findNumbers (readFile "/dev/stdin")

  let terrain = terrains depth targy targx
  print (sum ([terrain ! (y, x) | y <- [0 .. targy], x <- [0 .. targx]]))

  when (terrain ! (targy, targx) == torch) $ error "impossible"

  let width = targy + targx
  print (fromJust (astarInt (compress width) (neigh terrain) (heur targy targx) (== ((targy, targx), torch)) ((0, 0), torch)))

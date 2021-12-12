import Data.Array.IArray ((!), Array, array)
import Data.Char (isDigit)
import Data.List (maximumBy)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Text.Printf (printf)

size :: Int
size = 300

maxOfSize :: Int -> Array (Int, Int) Int -> (Int, (Int, Int))
maxOfSize sz sums = (val, (xbest + 1, ybest + 1))
  where (val, (xbest, ybest)) = maximumBy (comparing fst) cands
        squareSum (x, y) = sums ! (x + sz, y + sz) - sums ! (x + sz, y) - sums ! (x, y + sz) + sums ! (x, y)
        cands = [(squareSum (x, y), (x, y)) | x <- [0 .. size - sz], y <- [0 .. size - sz]]

-- TODO: Could use information from smaller sizes to eliminate larger sizes
maxOfAnySize :: Array (Int, Int) Int -> (Int, (Int, (Int, Int)))
maxOfAnySize sums = maximumBy (comparing (fst . snd)) [(sz, maxOfSize sz sums) | sz <- [1 .. size]]

summedAreaTable :: Int -> Array (Int, Int) Int
summedAreaTable serial = a
  where a = array ((0, 0), (size, size)) (zx ++ zy ++ rest)
        zx = [((x, 0), 0) | x <- [0 .. size]]
        zy = [((0, y), 0) | y <- [1 .. size]]
        rest = [((x, y), power serial x y + a ! (x, y - 1) + a ! (x - 1, y) - a ! (x - 1, y - 1)) | x <- [1 .. size], y <- [1 .. size]]

power :: Int -> Int -> Int -> Int
power serial x y = hundreds (((y * rack) + serial) * rack) - 5
  where rack = x + 10
        hundreds z = (z `quot` 100) `rem` 10

main :: IO ()
main = do
  args <- getArgs
  serial <- case args of
    [] -> fmap read (readFile "/dev/stdin")
    a:_ | all isDigit a -> return (read a)
    a:_ -> fmap read (readFile a)
  let sums = summedAreaTable serial
      (_, (x3, y3)) = maxOfSize 3 sums
  printf "%d,%d\n" x3 y3

  let (sz, (_, (x, y))) = maxOfAnySize sums
  printf "%d,%d,%d\n" x y sz

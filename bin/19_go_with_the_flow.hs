import AdventOfCode (readInputFile)
import AdventOfCode.Assembly (Inst, exec, inst, regsFromList)

import Data.Array ((!), Array, bounds, inRange, listArray)
import Data.Char (digitToInt)
import qualified Data.IntMap as IntMap
import Data.List (foldl')

run :: Array Int Inst -> Int -> Int -> Int -> Int
run insts pcreg target a = run' (regsFromList (a : replicate 5 0)) 0
  where run' regs pc | not (bounds insts `inRange` pc) = regs IntMap.! 0
        run' regs 1 = sumFactors (regs IntMap.! target)
        run' regs pc = run' regs' pc'
          where regs' = exec (insts ! pc) (IntMap.insert pcreg pc regs)
                pc' = 1 + regs' IntMap.! pcreg

sumFactors :: Int -> Int
sumFactors n0 = foldl' (*) 1 (primeFactors n0 candidates)
  where primeFactors 1 _ = []
        primeFactors n (x:xs) | n `rem` x /= 0 = primeFactors n xs
        primeFactors n (x:xs) = let (p, n') = usePrime x 1 x n in p : primeFactors n' xs
        primeFactors _ [] = error "impossible you can't run out of primes"
        usePrime prime primeSum primePower n | n `rem` primePower /= 0 = (primeSum, (n * prime) `quot` primePower)
        usePrime prime primeSum primePower n = usePrime prime (primeSum + primePower) (primePower * prime) n

candidates :: [Int]
candidates = 2 : 3 : concat [[6 * k - 1, 6 * k + 1] | k <- [1..]]

main :: IO ()
main = do
  s <- readInputFile
  let (pcreg, prog) = case lines s of
        (('#':'i':'p':' ':[n]):is) -> (digitToInt n, map inst is)
        _ -> error "bad prog"
      progArr = listArray (0, length prog - 1) prog
      (_, _, _, target) = reverse prog !! 2
  print (run progArr pcreg target 0)
  print (run progArr pcreg target 1)

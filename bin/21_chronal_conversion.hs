import AdventOfCode (readInputFile)
import AdventOfCode.Assembly (Inst, Op(Eqrr, Setr), exec, inst, regsFromList)

import Data.Array ((!), Array, bounds, inRange, listArray)
import Data.Char (digitToInt)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

run :: Array Int Inst -> Int -> [Int]
run insts pcreg = run' (regsFromList (replicate 6 0)) 0 IntSet.empty
  where divDest = case insts ! 26 of
          (Setr, _, _, d) -> d
          _ -> error "not setr"
        add r regs pc seen d =
          let v = regs IntMap.! r in
          if v `IntSet.member` seen then []
          else v : run' (IntMap.insert d 0 regs) (pc + 1) (IntSet.insert v seen)
        run' _ pc _ | not (bounds insts `inRange` pc) = []
        run' regs 17 seen =
          let v = regs IntMap.! divDest in
          run' (IntMap.insert divDest (v `quot` 256) regs) 27 seen
        run' regs pc seen = case insts ! pc of
          (Eqrr, r, 0, d) -> add r regs pc seen d
          (Eqrr, 0, r, d) -> add r regs pc seen d
          _ -> run' regs' pc' seen
            where regs' = exec (insts ! pc) (IntMap.insert pcreg pc regs)
                  pc' = 1 + regs' IntMap.! pcreg

main :: IO ()
main = do
  s <- readInputFile
  let (pcreg, prog) = case lines s of
        (('#':'i':'p':' ':[n]):is) -> (digitToInt n, map inst is)
        _ -> error "bad prog"
      progArr = listArray (0, length prog - 1) prog
      zeroes = run progArr pcreg
  print (head zeroes)
  print (last zeroes)

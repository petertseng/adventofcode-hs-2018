import AdventOfCode (readInputFile)

import Data.List (mapAccumL)

readTree :: ([Int] -> [Int] -> Int) -> [Int] -> ([Int], Int)
readTree f (nchild:nmeta:xsAfterCount) = (xsAfterMeta, f meta children)
  where (xsAfterChildren, children) = mapAccumL (\xs _ -> readTree f xs) xsAfterCount [1 .. nchild]
        (meta, xsAfterMeta) = splitAt nmeta xsAfterChildren
readTree _ _ = error "bad"

f1 :: [Int] -> [Int] -> Int
f1 meta children = sum (meta ++ children)

f2 :: [Int] -> [Int] -> Int
f2 meta [] = sum meta
f2 meta children = sum (map f2' meta)
  where f2' 0 = 0
        f2' n | n > length children = 0
        f2' n = children !! (n - 1)

main :: IO ()
main = do
  s <- readInputFile
  let nums = map read (words s)
  print (snd (readTree f1 nums))
  print (snd (readTree f2 nums))

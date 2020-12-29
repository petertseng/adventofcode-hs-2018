import AdventOfCode (readInputFile)

import Data.List (sort)

repeatFreq :: Int -> [Int] -> Int
repeatFreq freq xs = trd (minimum diffs)
  where cumulativeSum = scanl1 (+) xs
        byMod = sort (zipWith (\x i -> (x `mod` freq, x, i)) cumulativeSum [0 :: Int ..])
        diffs = [(val - prevVal, prevI, val) | ((m, prevVal, prevI), (m', val, _)) <- zip byMod (tail byMod), m == m']

trd :: (a, b, c) -> c
trd (_, _, x) = x

readPlus :: String -> Int
readPlus ('+':xs) = read xs
readPlus s = read s

main :: IO ()
main = do
  s <- readInputFile
  let nums = map readPlus (lines s)
      freq = sum nums
  print freq
  print (if freq == 0 then 0 else repeatFreq freq nums)

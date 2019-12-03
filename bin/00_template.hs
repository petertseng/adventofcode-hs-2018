import AdventOfCode (readInputFile)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  print (count (== "42") (lines s))

import AdventOfCode (readInputFile)

import Data.Char (ord, toUpper)
import Data.List (dropWhileEnd)

capital :: Int
capital = abs (ord 'A' - ord 'a')

react :: Char -> Char -> String -> String
react del1 del2 = react' []
  where react' s "" = s
        react' s1 (c:s2) | c == del1 || c == del2 = react' s1 s2
        react' (c1:s1) (c2:s2) | abs (ord c1 - ord c2) == capital = react' s1 s2
        react' s1 (c2:s2) = react' (c2:s1) s2

main :: IO ()
main = do
  s <- readInputFile
  let reacted = react ' ' ' ' (dropWhileEnd (== '\n') s)
  print (length reacted)

  let lengths = map (\c -> length (react c (toUpper c) reacted)) ['a' .. 'z']
  print (minimum lengths)

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Foldable (for_)
import qualified Data.Set as Set

type Star = ((Int, Int), (Int, Int))

yrangeAt :: Int -> [Star] -> Int
yrangeAt t = range . map (snd . posAt t)

posAt :: Int -> Star -> (Int, Int)
posAt t ((x, y), (vx, vy)) = (x + vx * t, y + vy * t)

range :: (Num a, Ord a) => [a] -> a
range xs = maximum xs - minimum xs

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f low high
  | low > high = high
  | f mid = bsearch f low (mid - 1)
  | otherwise = bsearch f (mid + 1) high
  where mid = low + ((high - low) `div` 2)

star :: String -> Star
star = (bracketed *** bracketed) . splitOnOne 'v'

bracketed :: String -> (Int, Int)
bracketed s = (read . dropWhile (== ' ') *** read) (splitOnOne ',' between)
  where (l, _) = splitOnOne '>' s
        (_, between) = splitOnOne '<' l

main :: IO ()
main = do
  s <- readInputFile
  let stars = map star (lines s)
      minTime = 1 + bsearch (\t -> yrangeAt t stars < yrangeAt (t + 1) stars) 0 (yrangeAt 0 stars)

  let stars' = map (posAt minTime) stars
      (xs, ys) = unzip stars'
      starSet = Set.fromList stars'

  for_ [minimum ys .. maximum ys] $ \y -> do
    let xs' = [minimum xs .. maximum xs]
    putStrLn (map (\x -> if (x, y) `Set.member` starSet then '#' else ' ') xs')

  putStrLn ""
  print minTime

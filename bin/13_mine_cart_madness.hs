import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), UArray, listArray)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq, Ord, Show)
type Cart = (Pos, Dir, Int)

-- (active carts in move order, crashes (most recent first))
tick :: UArray Pos Char -> ([Cart], [Pos]) -> ([Cart], [Pos])
tick g (carts, _) = tryMove (sort carts) [] []
  where tryMove [] moved crashes = (moved, crashes)
        tryMove (c:cs) moved crashes =
          let newCart@(newPos, _, _) = move g c
              samePos (pos, _, _) = pos == newPos
          in if any samePos (cs ++ moved)
            then tryMove (filter (not . samePos) cs) (filter (not . samePos) moved) (newPos : crashes)
            else tryMove cs (newCart:moved) crashes

move :: UArray Pos Char -> Cart -> Cart
move g ((y, x), d@(Dir (dy, dx)), t) = (p', c', t')
  where p' = (y + dy, x + dx)
        (c', t') = case (g ! p', t) of
          ('/', _) -> (Dir (-dx, -dy), t)
          ('\\', _) -> (Dir (dx, dy), t)
          ('+', 0) -> (turnLeft d, 1)
          ('+', 1) -> (d, 2)
          ('+', 2) -> (turnRight d, 0)
          _ -> (d, t)

turnLeft :: Dir -> Dir
turnLeft (Dir (dy, dx)) = Dir (-dx, dy)

turnRight :: Dir -> Dir
turnRight (Dir (dy, dx)) = Dir (dx, -dy)

cart :: (Pos, Char) -> Maybe Cart
cart (p, '^') = Just (p, Dir (-1, 0), 0)
cart (p, 'v') = Just (p, Dir (1, 0), 0)
cart (p, '>') = Just (p, Dir (0, 1), 0)
cart (p, '<') = Just (p, Dir (0, -1), 0)
cart _ = Nothing

grid :: String -> UArray Pos Char
grid s = listArray ((0, 0), (h - 1, w - 1)) (concat ls)
  where ls = lines s
        h = length ls
        w = uniform length ls

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

len1or0 :: [a] -> Bool
len1or0 [] = True
len1or0 [_] = True
len1or0 (_:_) = False

main :: IO ()
main = do
  s <- readInputFile
  let carts = mapMaybe cart (enumGrid (lines s))
      g = grid s
      (_, crashes) = until (not . null . snd) (tick g) (carts, [])
      -- crashes is in reverse order, so first crash is last in the list
      (y, x) = last crashes
  printf "%d,%d\n" x y

  case until (len1or0 . fst) (tick g) (carts, []) of
    ([((y2, x2), _, _)], _) -> printf "%d,%d\n" x2 y2
    ([], _) -> error "no carts left"
    (_:_, _) -> error "too many carts left"

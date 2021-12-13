module AdventOfCode.Assembly (
  Op(..),
  Regs,
  exec,
  regsFromList,
) where

import Data.Bits ((.&.), (.|.))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Regs = IntMap Int
type Inst = (Op, Int, Int, Int)
data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr deriving (Enum, Eq, Ord, Show)
data Mode = Immed | Reg

exec :: Inst -> Regs -> Regs
exec (Addr, a, b, c) = binop (+) Reg Reg a b c
exec (Addi, a, b, c) = binop (+) Reg Immed a b c
exec (Mulr, a, b, c) = binop (*) Reg Reg a b c
exec (Muli, a, b, c) = binop (*) Reg Immed a b c
exec (Banr, a, b, c) = binop (.&.) Reg Reg a b c
exec (Bani, a, b, c) = binop (.&.) Reg Immed a b c
exec (Borr, a, b, c) = binop (.|.) Reg Reg a b c
exec (Bori, a, b, c) = binop (.|.) Reg Immed a b c
exec (Setr, a, _, c) = binop const Reg undefined a undefined c
exec (Seti, a, _, c) = binop const Immed undefined a undefined c
exec (Gtir, a, b, c) = binop (btoi (>)) Immed Reg a b c
exec (Gtri, a, b, c) = binop (btoi (>)) Reg Immed a b c
exec (Gtrr, a, b, c) = binop (btoi (>)) Reg Reg a b c
exec (Eqir, a, b, c) = binop (btoi (==)) Immed Reg a b c
exec (Eqri, a, b, c) = binop (btoi (==)) Reg Immed a b c
exec (Eqrr, a, b, c) = binop (btoi (==)) Reg Reg a b c

binop :: (Int -> Int -> Int) -> Mode -> Mode -> Int -> Int -> Int -> Regs -> Regs
binop f m1 m2 a b c rs = IntMap.insert c (f va vb) rs
  where va = case m1 of
          Immed -> a
          Reg -> rs IntMap.! a
        vb = case m2 of
          Immed -> b
          Reg -> rs IntMap.! b

btoi :: (Int -> Int -> Bool) -> Int -> Int -> Int
btoi f a b = if f a b then 1 else 0

regsFromList :: [Int] -> Regs
regsFromList = IntMap.fromAscList . zip [0..]

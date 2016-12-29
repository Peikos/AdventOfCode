module Main where

import Data.List
import Data.List.Split
import qualified Data.Set as Set

input = splitOn ", " "L1, R3, R1, L5, L2, L5, R4, L2, R2, R2, L2, R1, L5, R3, L4, L1, L2, R3, R5, L2, R5, L1, R2, L5, R4, R2, R2, L1, L1, R1, L3, L1, R1, L3, R5, R3, R3, L4, R4, L2, L4, R1, R1, L193, R2, L1, R54, R1, L1, R71, L4, R3, R191, R3, R2, L4, R3, R2, L2, L4, L5, R4, R1, L2, L2, L3, L2, L1, R4, R1, R5, R3, L5, R3, R4, L2, R3, L1, L3, L3, L5, L1, L3, L3, L1, R3, L3, L2, R1, L3, L1, R5, R4, R3, R2, R3, L1, L2, R4, L3, R1, L1, L1, R5, R2, R4, R5, L1, L1, R1, L2, L4, R3, L1, L3, R5, R4, R3, R3, L2, R2, L1, R4, R2, L3, L4, L2, R2, R2, L4, R3, R5, L2, R2, R4, R5, L2, L3, L2, R5, L4, L2, R3, L5, R2, L1, R1, R3, R3, L5, L2, L2, R5"

data Turn = L | R -- L Int | R Int
  deriving Show

type Instruction = (Turn, Int)

instruction :: String -> Instruction
instruction ('L' : i) = (L, read i)
instruction ('R' : i) = (R, read i)

instructions = map instruction input

data Direction = N | E | S | W
  deriving (Show, Eq, Enum, Bounded)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add mod x y = (x + y + mod) `rem` mod

debug :: [Instruction] -> [(Direction, (Int, Int))] -- -> (Direction, (Int, Int))
debug []             = [(N, (0,0))]
debug ((t, dist):is) = (zip (repeat d') $ reverse cs) ++ debug is
  where (d, (x, y)) = head $ debug is
        d' = case t of
          L -> prev d
          R -> next d
        cs = case d' of
          N -> [(x,y+i) | i <- [1..dist]]
          E -> [(x+i,y) | i <- [1..dist]]
          S -> [(x,y-i) | i <- [1..dist]]
          W -> [(x-i,y) | i <- [1..dist]]

step :: [Instruction] -> (Direction, (Int, Int)) -- -> (Direction, (Int, Int))
step []             = (N, (0,0))
step ((t, dist):is) = (d', (x', y'))
  where (d, (x, y)) = step is
        d' = case t of
          L -> prev d
          R -> next d
        (x', y') = case d' of
          N -> (x,y+dist)
          E -> (x+dist,y)
          S -> (x,y-dist)
          W -> (x-dist,y)

solve i = distance . snd $ step i

dup :: (Eq a, Ord a) => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s
                           then Just x
                           else dup' xs (Set.insert x s)

distance :: (Int, Int) -> Int
distance (x,y) = abs x + abs y

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

test1 = solve [(R,2),(L,3)]
test2 = solve [(R,2),(R,2),(R,2)]
test3 = solve [(R,5),(L,5),(R,5),(R,3)]

test = [test1, test2, test3]

result = solve . reverse $ instructions
result2 = fmap distance . dup . reverse . map snd . debug . reverse $ instructions

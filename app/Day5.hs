module Day5 where

import Control.Applicative
import Data.Bifunctor
import Data.List
import Data.Maybe
import Util

type Seat = ((Int, Int), (Int, Int))
data Direction = F | B | L | R deriving (Show, Read, Eq)

readData :: String -> [Direction]
readData = map (read . return)

allSeats = ((0,127), (0,7))

mid :: Int -> Int -> Int
mid a b = div (b - a) 2

upper, lower :: (Int, Int) -> (Int, Int)
upper  (a, b) = (a, b - mid a b - 1)
lower (a, b) = (a + mid a b + 1, b)

findSeat :: [Direction] -> Seat -> Seat
findSeat [] ((ra,rb),(ca,cb)) = ((ra,rb),(ca,cb))
findSeat (F:dirs) (row,seat) = findSeat dirs (upper row, seat)
findSeat (B:dirs) (row,seat) = findSeat dirs (lower row, seat)
findSeat (L:dirs) (row,seat) = findSeat dirs (row, upper seat)
findSeat (R:dirs) (row,seat) = findSeat dirs (row, lower seat)

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

unify :: Eq a => (a, a) -> Maybe a
unify (a, b) | a == b    = Just b
             | otherwise = Nothing

seatNum = fromMaybe 0 . uncurry (liftA2 (\r c -> r * 8 + c)) . both unify . flip findSeat allSeats . readData

challenge1 = maximum . map seatNum . lines

challenge2 input = let seats = ([0..1023] \\) . map seatNum . lines $ input
                       mask = zipWith (-) (tail seats) (seats)
                   in last . catMaybes . zipWith (\seat mask -> if mask == 1 then Nothing else Just seat) seats $ mask 

seatNum' = sum . zipWith (\e c -> v c * 2^e) [9,8..0] . readData
  where v F = 0
        v L = 0
        v _ = 1

challenge1' = maximum . map seatNum' . lines

-- challenge2 input = let seats = ([0..1023] \\) . map seatNum . lines $ input
                       -- mask = map (/= -1) $ mapAdjacent (-) seats
                   -- in last . filterMask mask $ seats

-- challenge2' = last . (flip filterMask <*> map (/= -1) . mapAdjacent (-))
                   -- . ([0..1023] \\) . map seatNum' . lines

challenge2' = last . (maskWith (map (/= -1) . mapAdjacent (-)))
                   . ([0..1023] \\) . map seatNum' . lines

main = do
  putStr "Input (first) =  "
  result $ challenge1 <$> readFile "data/input5"
  putStr "Alternative (first) =  "
  result $ challenge1' <$> readFile "data/input5"
  putStr "Input (second) =  "
  result $ challenge2 <$> readFile "data/input5"
  putStr "Alternative (second) =  "
  result $ challenge2' <$> readFile "data/input5"

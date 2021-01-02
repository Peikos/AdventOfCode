module Day3 where

import Util

data Square = Tree | Ground
type Line = [Square]
type Map = [Line]
data Slope = Slope Int Int

isTree :: Square -> Bool
isTree Tree = True
isTree _ = False

readMap :: String -> Map
readMap = map (map readTree . cycle) . lines
  where readTree '#' = Tree
        readTree '.' = Ground

keepEveryNth :: Int -> [b] -> [b]
keepEveryNth n = map snd . filter ((== n) . fst) . (zip $ cycle [1..n])

line :: Slope -> Map -> Int
line (Slope dx dy) = sum . zipWith f [dx,2*dx..] . keepEveryNth dy . tail
  where f :: Int -> Line -> Int
        f n l = if isTree (l !! n) then 1 else 0

challenge1 :: Map -> Int
challenge1 = line (Slope 3 1)

challenge2 :: Map -> Int
challenge2 m = product . map (flip line m) $ [(Slope 1 1), (Slope 3 1), (Slope 5 1), (Slope 7 1), (Slope 1 2)]

main = do
  putStr "Sample (first) =  "
  result $ challenge1 . readMap <$> readFile "data/sample3"
  putStr "Answer (first) =  "
  result $ challenge1 . readMap <$> readFile "data/input3"
  putStr "Sample (second) =  "
  result $ challenge2 . readMap <$> readFile "data/sample3"
  putStr "Answer (second) =  "
  result $ challenge2 . readMap <$> readFile "data/input3"

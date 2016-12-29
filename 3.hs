module Main where

import Data.List
import Data.List.Split

readInput :: IO [[Int]]
readInput = do f <- readFile "3.txt"
               return . (map ((map read) . words)) . lines $ f

main = do i <- readInput
          print i
          print . length $ i
          print . length . filter validTriangle $ i
          print . length . filter validTriangle . vertical $ i

validTriangle :: [Int] -> Bool
validTriangle = and . map (\sides -> (head sides) < (sum $ tail sides)) . permutations

vertical :: [[Int]] -> [[Int]]
vertical = concatMap (chunksOf 3) . transpose

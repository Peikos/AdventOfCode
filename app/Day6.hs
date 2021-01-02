module Day6 where

import Util
import Data.List

readData :: String -> [[String]]
readData = splitOn [] . lines

challenge1, challenge2 :: [[String]] -> Int
challenge1 = sum . map (length . nub . concat)
challenge2 = sum . map (length . foldr1 intersect)

main = do
  putStr "Sample (first) =  "
  result $ challenge1 . readData <$> readFile "data/sample6"
  putStr "Input (first) =  "
  result $ challenge1 . readData <$> readFile "data/input6"
  putStr "Sample (second) =  "
  result $ challenge2 . readData <$> readFile "data/sample6"
  putStr "Input (second) =  "
  result $ challenge2 . readData <$> readFile "data/input6"

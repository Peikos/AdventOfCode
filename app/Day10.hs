module Day10 where

import Util

import Control.Arrow
import Data.Graph
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

readData :: String -> [Int]
readData = map read . lines

challenge1 :: [Int] -> Int
challenge1 = uncurry (*) . (length . filter (== (-1)) &&& succ . length . filter (== (-3))) . mapAdjacent (-) . (0:) . sort

trib 0 = 1
trib 1 = 1
trib 2 = 2
trib n = trib (n-1) + trib (n-2) + trib (n-3)

challenge2 :: [Int] -> Int
challenge2 =  product . map (trib . length) . filter (\x -> head x == (-1)) . group . mapAdjacent (-) . (0:) . sort

main = do sample <- readData <$> readFile "data/sample10a"
          sample2 <- readData <$> readFile "data/sample10b"
          input <- readData <$> readFile "data/input10"

          putStrLn "Sample"
          print $ sample
          print $ challenge2 sample
          print $ challenge1 sample


          putStrLn "Sample 2"
          print $ sample2
          print $ challenge2 sample2
          print $ challenge1 sample2

          putStrLn "Input"
          print input
          print $ challenge1 input
          print $ challenge2 input

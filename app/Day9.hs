module Day9 where

import Util

import Control.Arrow
import Data.List
import Data.Maybe

readData :: String -> [Int]
readData = map read . lines

sections :: Int -> [Int] -> [[Int]]
sections preambleSize ints = take (length ints - preambleSize)  -- Ignore shorter lists at the end
                           . map (take $ succ preambleSize)     -- Only take first n items and desired sum
                           . tails $ ints                       -- All starting points

challenge1 :: Int -> [Int] -> Int
challenge1 preambleSize ints = head [last sec | sec <- sections preambleSize ints           -- Given all possible sections
                                              , not $ or [a + b == last sec | a <- init sec -- Keep those where two separate
                                                                            , b <- init sec --   numbers sum to the last 
                                                                            , a /= b]]      --   number in the section

challenge2 :: Int -> [Int] -> Int
challenge2 v = uncurry (+) . (minimum &&& maximum) . head    -- Sum largest and smallest numer in first answer
             . filter (\l -> length l > 1 && sum l == v)     -- Leave only those with nontrivial sum v
             . concatMap inits . tails                       -- All potential lists to sum

main = do sample <- readData <$> readFile "data/sample9"
          input <- readData <$> readFile "data/input9"

          putStrLn "Sample"
          let sample1 = challenge1 5 $ sample
          print sample1
          print $ challenge2 sample1 $ sample

          putStrLn "Input"
          let input1 = challenge1 25 $ input
          print input1
          print $ challenge2 input1 $ input

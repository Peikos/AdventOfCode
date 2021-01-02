module Day13 where

import Util

import Control.Arrow
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe

readData :: String -> (Integer, [Maybe Integer])
readData = (read . head &&& map maybeRead . splitOn ',' . head . tail) . lines
  where maybeRead "x" = Nothing
        maybeRead i   = Just $ read i

challenge1 :: (Integer, [Maybe Integer]) -> Integer
challenge1 (start, bs) = uncurry (*)                                      -- Multiply number and time
                       . minimumBy (compare `on` snd)                     -- Find lowest time to depart
                       . zipWith (\num timeAgo -> (num, num - timeAgo))   -- Calculate next departure and
                                 operableBuses                            --   keep id as well
                       . map (mod start)                                  -- How long since last depart
                       $ operableBuses                                    -- Remove defunct bussed
  where operableBuses = catMaybes bs

-- Naive approach, probably works but will take ages
naive :: (Integer, [Maybe Integer]) -> Integer
naive (_, buses) = head $ filter (valid buses) [0..]

valid :: [Maybe Integer] -> Integer -> Bool
valid buses ts = all ($ ts) $ zipWith validator buses [0..]
  where validator :: Maybe Integer -> Integer -> Integer -> Bool
        validator Nothing _ _ = False
        validator (Just busId) base offset = mod (base + offset) busId == 0

-- Works for all examples except the actual input. Runs for one step too many (second last answer turned out to be correct... almost. Off by one. No idea.

challenge2' :: (Integer, [Maybe Integer]) -> Integer
challenge2' = go 0 . reverse . snd
  where
    go :: Integer -> [Maybe Integer] -> Integer
    go s (Just a:Nothing:rest) =  go (s+a-1) (Just a:rest)
    go s (Just a:Just b:rest) = let start = takeFirst (\x -> x `mod` b == 0) [s+a-1, s+2*a-1..]
                                    period = a * b
                                in  go start (Just period:rest)
    go s _ = s

-- Based on Chinese remainder theorem
challenge2 :: (Integer, [Maybe Integer]) -> Integer
challenge2 (_, buses) = (flip mod totalProd) . sum . zipWith f [0,-1..] $ buses
  where totalProd = product $ catMaybes $ buses
        f :: Integer -> Maybe Integer -> Integer
        f rest (Just bus) = let otherPrimesProd = (totalProd `div` bus)
                          in rest * otherPrimesProd * (findMultiple (otherPrimesProd `mod` bus) bus)
        f _ Nothing = 0
findMultiple :: Integer -> Integer -> Integer
findMultiple num modulo = takeFirst (\x -> (x*num) `mod` modulo == 1) [1..]

main = do putStrLn "Sample data"
          sample <- readData <$> readFile "data/sample13"
          print $ challenge1 sample
          print $ challenge2 sample
          putStrLn ""

          putStrLn "Input data"
          input <- readData <$> readFile "data/input13"
          print $ challenge1 input
          print $ challenge2 input
          putStr "Own solution: "
          print $ challenge2' input
          putStrLn ""

          putStrLn "Comparing CRT to own solution for extra sample inputs:"
          print $ [ challenge2' (0, [Just 17, Nothing, Just 13, Just 19])
                  , challenge2 (0, [Just 17, Nothing, Just 13, Just 19])
                  , challenge2' (0, [Just 67, Just 7, Just 59, Just 61])
                  , challenge2 (0, [Just 67, Just 7, Just 59, Just 61])
                  , challenge2' (0, [Just 67,Nothing,  Just 7, Just 59, Just 61])
                  , challenge2 (0, [Just 67,Nothing,  Just 7, Just 59, Just 61])
                  , challenge2' (0, [Just 67, Just 7,Nothing, Just 59, Just 61])
                  , challenge2 (0, [Just 67, Just 7,Nothing, Just 59, Just 61])
                  , challenge2' (0, [Just 1789, Just 37, Just 47, Just 1889])
                  , challenge2 (0, [Just 1789, Just 37, Just 47, Just 1889])
                  ]



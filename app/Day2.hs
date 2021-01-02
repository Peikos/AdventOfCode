module Day2 where

import Util
import Parser
import Data.Bool.Unicode ((⊻))

sample_input = [ "1-3 a: abcde"
               , "1-3 b: cdefg"
               , "2-9 c: ccccccccc"
               ]

challenge1 :: [String] -> Int
challenge1 = count id . filter id . map (maybe False valid1 . split)
  where valid1 :: (Char, Int, Int, String) -> Bool
        valid1 (c, min, max, pass) = within (min, max) $ count (== c) pass


challenge2 :: [String] -> Int
challenge2 = count id . filter id . map (maybe False valid2 . split)
  where valid2 :: (Char, Int, Int, String) -> Bool
        valid2 (c, p1, p2, pass) = atPos p1 c pass ⊻ atPos p2 c pass

{-
challenge1 :: [String] -> Int
challenge1 = count True . filter id . map (maybe False valid1 . split)
  where valid1 :: (Char, Int, Int, String) -> Bool
        valid1 (c, min, max, pass) = within (min, max) $ count c pass


challenge2 :: [String] -> Int
challenge2 = count True . filter id . map (maybe False valid2 . split)
  where valid2 :: (Char, Int, Int, String) -> Bool
        valid2 (c, p1, p2, pass) = atPos p1 c pass ⊻ atPos p2 c pass

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)
-}

atPos :: Eq a => Int -> a -> [a] -> Bool
atPos p c = (== c) . head . drop (pred p)

split :: String -> Maybe (Char, Int, Int, String)
split = parse pLine
  where pLine = do min <- pInteger
                   _ <- pChar '-'
                   max <- pInteger
                   _ <- pChar ' '
                   char <- pAny
                   _ <- pString ": "
                   pass <- pMany
                   return (char, min, max, pass)

main = do
  putStr "Sample (first) =  "
  print . challenge1 $ sample_input
  putStr "Answer (first) =  "
  chal1 <- challenge1 . lines <$> readFile "data/input2"
  print chal1
  putStr "Sample (second) = "
  print . challenge2 $ sample_input
  putStr "Answer (second) = "
  chal2 <- challenge2 . lines <$> readFile "data/input2"
  print chal2

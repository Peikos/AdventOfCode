{-# LANGUAGE TypeApplications #-}

module Day1 where

sample_input = [1721,979,366,299,675,1456]

main = do
  input <- map (read @Int) . lines <$> readFile "data/input1"
  print $ [(a,b,a*b) | a <- input, b <- input, a < b, a + b == 2020]
  print $ [(a,b,c,a*b*c) | a <- input, b <- input, c <- input, a < b, b < c, a + b + c == 2020]

module Main where

import Data.List
import GHC.Exts

getInput :: IO [String]
getInput = do s <- readFile "6.txt"
              return $ lines s

test :: [String]
test = ["eedadn" ,"drvtee" ,"eandsr" ,"raavrd" ,"atevrs" ,"tsrnev" ,"sdttsa" ,"rasrtv" ,"nssdts" ,"ntnada" ,"svetve" ,"tesnvt" ,"vntsnd" ,"vrdear" ,"dvrsen" ,"enarar"]

frequents = map (head . last . sortWith length . group . sort) . transpose
infrequents = map (head . head . sortWith length . group . sort) . transpose

main = do input <- getInput
          print . frequents $ test
          print . infrequents $ test
          print . frequents $ input
          print . infrequents $ input

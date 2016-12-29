module Main where

import Data.List
import Data.Maybe
import GHC.Exts

digit :: Char -> Bool
digit = flip elem "1234567890"

letter :: Char -> Bool
letter = flip elem "qdrwbjfupashtgyneoizxmcvkl"

parseLine :: String -> (String, Int, String)
parseLine s = let checksum = init . tail . dropWhile (/= '[') $ s
                  sector = read . takeWhile digit . dropWhile (not . digit) $ s
                  string = init . takeWhile (not . digit) $ s
              in (string, sector, checksum)

room :: (String, Int, String) -> Maybe (Int, String)
room (s,i,c) | checksum s' == c = Just (i, s)
             | otherwise        = Nothing
  where s' = filter (/= '-') s

checksum :: String -> String
checksum = take 5 . map head . concatMap (sortWith head) . groupWith ((0-) . length) . sortWith length . group . sort

rooms = catMaybes . map (room . parseLine)
total = sum . map fst . rooms

decode :: (Int, String) -> (Int, String)
decode (i, s) = (i, map f s)
  where f x | letter x = toEnum . (\x -> 97 + (i+x-97) `mod` 26) . fromEnum $ x
            | x == '-' = ' '
            | otherwise = undefined

main :: IO ()
main = do print $ rooms test
          input <- readFile "4.txt"
          print . map decode . rooms . lines $ input
          print . total . lines $ input
          print . find ((== "north") . take 5 . snd) . map decode . rooms . lines $ input

test = ["aaaaa-bbb-z-y-x-123[abxyz]", "a-b-c-d-e-f-g-h-987[abcde]", "not-a-real-room-404[oarel]", "totally-real-room-200[decoy]"]

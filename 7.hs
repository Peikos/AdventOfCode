module Main where

import Data.Maybe
import Data.List

test = ["abba[mnop]qrst", "abcd[bddb]xyyx", "aaaa[qwer]tyui", "ioxxoj[asdfgh]zxcvbn"]
test2 = ["aba[bab]xyz", "xyx[xyx]xyx", "aaa[kek]eke", "zazbz[bzb]cdb"]

getInput :: IO [String]
getInput = do s <- readFile "7.txt"
              return $ lines s

hypernet :: String -> String
hypernet = takeWhile (/= ']') . tail . dropWhile (/= '[')

hypernets :: String -> [String]
hypernets = words . o
  where o (c:cs) | c == '['  = i cs
                 | otherwise = o cs
        o []                 = []
        i (c:cs) | c == ']'  = ' ' : o cs
                 | otherwise = c   : i cs
        i []                 = []

supernets :: String -> [String]
supernets = words . o
  where o (c:cs) | c == '['  = ' ' : i cs
                 | otherwise = c   : o cs
        o []                 = []
        i (c:cs) | c == ']'  = o cs
                 | otherwise = i cs
        i []                 = []

subs :: Int -> String -> [String]
subs n s | length s >= n = take n s : subs n (tail s)
         | otherwise = []

abba :: String -> Bool
abba = or . map smallPalindrome . subs 4

smallPalindrome :: String -> Bool
smallPalindrome s = s == reverse s && s !! 0 /= s !! 1

tls :: String -> Bool
tls s = abba s && (not . or . map abba . hypernets $ s)

ssl :: String -> Bool
ssl i = let abas = filter smallPalindrome . concatMap (subs 3) . supernets $ i
            babs = map (\x -> map (x !!) [1,2,1]) abas
        in isJust . find (flip elem babs) . concatMap (subs 3) . hypernets $ i

main = do input <- getInput
          print . map tls $ test
          --print . filter tls $ input
          print . length . filter tls $ input
          print . map ssl $ test2
          --print . filter ssl $ input
          print . length . filter ssl $ input

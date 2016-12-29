{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Digest.Pure.MD5
import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as BS

input = "abbhdwsy"
test = "abc"

hashes = map (show . md5 . BS.pack) . zipWith (flip (++)) (map show [1..]) . repeat
relevant = filter ((== "00000") . take 5) . hashes

password :: String -> String
password = take 8 . map (!! 5) . relevant

password' :: String -> String
password' i = catMaybes . map (flip lookup chars) $ "01234567"
  where chars :: [(Char, Char)]
        chars = map (\x -> (x !! 5, x !! 6)) . relevant $ i

main = do putStrLn "== Input =="
          print $ password input
          print $ password' input
          putStrLn "== Test Data =="
          print $ password test
          print $ password' test

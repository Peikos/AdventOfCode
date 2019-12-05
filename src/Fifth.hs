{-# LANGUAGE OverloadedStrings #-}

module Fifth  where

import Data.Text (splitOn)

import Computer

input :: IO Program
input = toStore . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input5"

test0,test1,test2,test3,test4,test5,test6,test7 :: Program
test0 = toStore $ map Just [3,0,4,0,99]
test1 = toStore $ map Just [3,9,8,9,10,9,4,9,99,-1,8]
test2 = toStore $ map Just [3,9,7,9,10,9,4,9,99,-1,8]
test3 = toStore $ map Just [3,3,1108,-1,8,3,4,3,99]
test4 = toStore $ map Just [3,3,1107,-1,8,3,4,3,99]
test5 = toStore $ map Just [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
test6 = toStore $ map Just [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
test7 = toStore $ map Just [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31
                           ,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104
                           ,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

runTest :: Program -> Maybe [Value]
runTest = sequence . snd . eval [8]

firstAnswer, secondAnswer :: IO (Maybe [Int])
firstAnswer = sequence . snd . eval [1] <$> input
secondAnswer = sequence . snd . eval [5] <$> input

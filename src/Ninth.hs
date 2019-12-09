{-# LANGUAGE OverloadedStrings #-}

module Ninth  where

import Data.Text (splitOn)

import Computer

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input9"

runTest :: ComputerState -> Maybe [Value]
runTest = sequence . snd . eval []

runWith :: Text -> [Value] -> ComputerState -> (ComputerState, [Maybe Value])
runWith name = eval' (computer name)

runCont :: Text -> [Value] -> ComputerState -> (ComputerState, [Maybe Value])
runCont name = eval' (nonStop name)

test0, test1, test2 :: ComputerState
test0 = loadProgram [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
test1 = loadProgram [1102,34915192,34915192,7,4,7,99,0]
test2 = loadProgram [104,1125899906842624,99]

firstAnswer, secondAnswer :: IO [Maybe Value]
firstAnswer = snd . runCont "" [1] <$> input
secondAnswer = snd . runCont "" [2] <$> input

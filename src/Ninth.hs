{-# LANGUAGE OverloadedStrings #-}

module Ninth (d9p1, d9p2, d9t0, d9t1, d9t2) where

import Data.Text (splitOn)

import Computer

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input9"

d9t0, d9t1, d9t2 :: ComputerState
d9t0 = loadProgram [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
d9t1 = loadProgram [1102,34915192,34915192,7,4,7,99,0]
d9t2 = loadProgram [104,1125899906842624,99]

d9p1, d9p2 :: IO [Maybe Value]
d9p1 = snd . runCont "" [1] <$> input
d9p2 = snd . runCont "" [2] <$> input

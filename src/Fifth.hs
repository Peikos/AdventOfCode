{-# LANGUAGE OverloadedStrings #-}

module Fifth (d5p1, d5p2, d5t0, d5t1, d5t2, d5t3, d5t4, d5t5, d5t6, d5t7) where

import Data.Text (splitOn)

import Computer

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input5"

d5t0, d5t1, d5t2, d5t3, d5t4, d5t5, d5t6, d5t7 :: ComputerState
d5t0 = loadProgram [3,0,4,0,99]
d5t1 = loadProgram [3,9,8,9,10,9,4,9,99,-1,8]
d5t2 = loadProgram [3,9,7,9,10,9,4,9,99,-1,8]
d5t3 = loadProgram [3,3,1108,-1,8,3,4,3,99]
d5t4 = loadProgram [3,3,1107,-1,8,3,4,3,99]
d5t5 = loadProgram [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
d5t6 = loadProgram [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
d5t7 = loadProgram [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0
                   ,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46
                   ,1101,1000,1,20,4,20,1105,1,46,98,99]

d5p1, d5p2 :: IO (Maybe [Int])
d5p1 = sequence . snd . eval' (nonStop "") [1] <$> input
d5p2 = sequence . snd . eval [5] <$> input

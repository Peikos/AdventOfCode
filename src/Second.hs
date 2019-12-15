{-# LANGUAGE OverloadedStrings #-}

module Second (d2p1, d2p2, d2t0, d2t1, d2t2, d2t3, d2t4) where

import Data.Text (splitOn)
import Relude.Extra.Map (lookup)
import WStore

import Computer

type Result = Maybe Int

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input2"

alterMem :: Value -> Value -> ComputerState -> ComputerState
alterMem a b = memOp (wWrite 1 (Just a) . wWrite 2 (Just b))

nounVerbMap :: ComputerState -> Map Result (Value, Value)
nounVerbMap p = fromList [ (result $ eval [] $ alterMem noun verb p, (noun, verb))
                         | noun <- [0..99]
                         , verb <- [0..99]
                         ]

result :: (ComputerState, [Maybe Value]) -> Result
result = wPeek 0 . cMemory . fst

d2t0, d2t1, d2t2, d2t3, d2t4 :: ComputerState
d2t0 = loadProgram [1,9,10,3,2,3,11,0,99,30,40,50]
d2t1 = loadProgram [1,0,0,0,99]
d2t2 = loadProgram [2,3,0,3,99]
d2t3 = loadProgram [2,4,4,5,99,0]
d2t4 = loadProgram [1,1,1,4,99,5,6,0,99]

d2p1, d2p2 :: IO Result
d2p1 = result . eval [] . alterMem 12 2 <$> input
d2p2 = fmap (\(n,v) -> 100 * n + v) . lookup (Just 19690720) . nounVerbMap
   <$> input

{-# LANGUAGE OverloadedStrings #-}

module Second  where

import Control.Comonad.Store (peek)
import Data.Text (splitOn)
import Relude.Extra.Map (lookup)

import Computer

type Result = Maybe Int

input :: IO Program
input = toStore . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input2"

alterMem :: Value -> Value -> Program -> Program
alterMem a b = write 1 (Just a) . write 2 (Just b)

nounVerbMap :: Program -> Map Result (Value, Value)
nounVerbMap p = fromList [ (result $ eval [] $ alterMem noun verb p, (noun, verb))
                         | noun <- [0..99]
                         , verb <- [0..99]
                         ]

result :: (Program, [Maybe Value]) -> Result
result = peek 0 . fst

test0, test1, test2, test3, test4 :: Program
test0 = toStore $ map Just [1,9,10,3,2,3,11,0,99,30,40,50]
test1 = toStore $ map Just [1,0,0,0,99]
test2 = toStore $ map Just [2,3,0,3,99]
test3 = toStore $ map Just [2,4,4,5,99,0]
test4 = toStore $ map Just [1,1,1,4,99,5,6,0,99]

firstAnswer, secondAnswer :: IO Result
firstAnswer = result . eval [] . alterMem 12 2 <$> input
secondAnswer = fmap (\(n,v) -> 100 * n + v) . lookup (Just 19690720)
             . nounVerbMap <$> input

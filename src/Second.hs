{-# LANGUAGE OverloadedStrings #-}

module Second  where

import Control.Comonad.Store (Store, store, runStore, peek, seeks, experiment)
import Data.Text (splitOn)
import Relude.Extra.Map (lookup)

type Program = Store Int (Maybe Int)
type Memory = [Int]
type Result = Maybe Int
type Address = Int
type Value = Int

input :: IO Program
input = toStore . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input2"

toStore :: [Maybe Value] -> Program
toStore = flip store 0 . (\xs -> join . atIdx xs)

fetch :: Program -> [Maybe Value]
fetch = experiment enumFrom

write :: Address -> Maybe Value -> Program -> Program
write key val st = store newLookup curPos
    where (oldLookup, curPos) = runStore st
          newLookup key' | key' == key = val
                         | otherwise   = oldLookup key'

op4 :: (Value -> Value -> Value)
    -> Address -> Address -> Address
    -> Program -> Program
op4 f a b r = do av <- peek a
                 bv <- peek b
                 write r (f <$> av <*> bv) >>> seeks (+4)

eval :: Program -> Program
eval st = case fetch st of
    (Just 1: Just a: Just b: Just r: _) -> eval $ op4 (+) a b r st
    (Just 2: Just a: Just b: Just r: _) -> eval $ op4 (*) a b r st
    (Just 99:_)                         -> st
    _                                   -> error "Invalid opcode!"

alterMem :: Value -> Value -> Program -> Program
alterMem a b = write 1 (Just a) . write 2 (Just b)

nounVerbMap :: Program -> Map Result (Value, Value)
nounVerbMap p = fromList [ (result $ eval $ alterMem noun verb p, (noun, verb))
                         | noun <- [0..99]
                         , verb <- [0..99]
                         ]

memDump :: Program -> Memory
memDump = catMaybes . takeWhile isJust . experiment (const [0..])

result :: Program -> Result
result = peek 0

test0, test1, test2, test3, test4 :: Program
test0 = toStore $ map Just [1,9,10,3,2,3,11,0,99,30,40,50]
test1 = toStore $ map Just [1,0,0,0,99]
test2 = toStore $ map Just [2,3,0,3,99]
test3 = toStore $ map Just [2,4,4,5,99,0]
test4 = toStore $ map Just [1,1,1,4,99,5,6,0,99]

firstAnswer, secondAnswer :: IO Result
firstAnswer = result . eval . alterMem 12 2 <$> input
secondAnswer = fmap (\(n,v) -> 100 * n + v) . lookup (Just 19690720)
             . nounVerbMap <$> input

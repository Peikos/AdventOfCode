{-# LANGUAGE OverloadedStrings #-}

module Seventh  where

import Data.Text (splitOn)
import Data.List (groupBy)

import Computer

type Output = Maybe (Maybe [Value], [[Value]])

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input7"

labels :: [Text]
labels = ["A","B","C","D","E"]

searchSpace :: [[Value]]
searchSpace = permutations [0..4]

searchSpace2 :: [[Maybe Value]]
searchSpace2 = permutations $ map Just [5..9]

runWith :: Text -> ComputerState -> [Value] -> (ComputerState, [Maybe Value])
runWith name p e = eval' (computer name) e p

selectBest :: [(Maybe [Value], [Value])] -> Output
selectBest = fmap (first (join . viaNonEmpty head) . unzip) . viaNonEmpty last
               <$> groupBy ((==) `on` fst) . sortOn fst

pushHead :: Maybe a -> [[Maybe a]] -> [[Maybe a]]
pushHead (Just x) (y:ys) = (y++[Just x]):ys
pushHead (Just x) [] = [[Just x]]
pushHead Nothing ys = ys

tryPerm :: Maybe [Value] -> ComputerState -> [Value] -> Maybe [Value]
tryPerm res _ [] = res
tryPerm oldRes p (s:ss) = do (_, r) <- runWith "" p <$> ((s:) <$> oldRes)
                             tryPerm (sequence r) p ss

tryPerms :: ComputerState -> Output
tryPerms p = selectBest . map (tryPerm (Just [0]) p &&& id) $ searchSpace

tryPerm2 :: ComputerState -> [Maybe Value] -> Maybe [Value]
tryPerm2 i perm = loop (zip labels $ replicate 5 i)
                       (pushHead (Just 0) . map singleton $ perm)
  where loop :: [(Text, ComputerState)] -> [[Maybe Value]] -> Maybe [Value]
        loop [] _ = error "No programs left to run"
        loop _ [] = error "No input available for next program"
        loop ((t, p):ps) (s:ss) =
            do let (p', r) = runWith t p (catMaybes s)
               if cHalted p'
                   then sequence s
                   else loop (ps++[(t, p')])
                             (pushHead (join $ viaNonEmpty head r) ss)

tryPerms2 :: ComputerState -> Output
tryPerms2 p = selectBest $ map (tryPerm2 p &&& catMaybes) searchSpace2

firstAnswer, secondAnswer :: IO Output
firstAnswer = tryPerms <$> input
secondAnswer = tryPerms2 <$> input

firstTest, secondTest, thirdTest, fourthTest, fifthTest :: Output
firstTest = tryPerms test0
secondTest = tryPerms test1
thirdTest = tryPerms test2
fourthTest = tryPerms2 test3
fifthTest = tryPerms2 test3

test0, test1, test2, test3, test4 :: ComputerState
test0 = loadProgram [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
                    -- 4,3,2,1,0 -> 43210

test1 = loadProgram [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23
                    ,23,4,23,99,0,0] -- 0,1,2,3,4 -> 54321

test2 = loadProgram [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33
                    ,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
                    -- 1,0,4,3,2 -> 65210

test3 = loadProgram [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001
                    ,28,-1,28,1005,28,6,99,0,0,5] -- 9,8,7,6,5 -> 139629729

test4 = loadProgram [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55
                    ,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001
                    ,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0
                    ,0,10] -- 9,7,8,5,6 -> 18216

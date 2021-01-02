module Day8 where

import Util
import Parser

import Control.Monad.Trans.RWS.Lazy (RWS, execRWS, tell)
import Control.Applicative
import Data.Maybe

import qualified Data.Map as Map

data Operation = Acc Int | Jmp Int | Nop Int deriving Show
data State = Running | OutOfBounds | Looped deriving Show

data Computer = Computer { iar :: Int
                         , ram :: Int
                         , text :: [Operation]
                         , visited :: [Int]
                         , state :: State
                         } deriving Show

parseSign = parsePlus <|> parseMinus
  where parsePlus = const 1 <$> pChar '+'
        parseMinus = const (-1) <$> pChar '-'

parseAcc :: Parser Operation
parseAcc = do pString "acc "
              s <- parseSign
              i <- pInteger
              return $ Acc (s * i)

parseNop :: Parser Operation
parseNop = do pString "nop "
              s <- parseSign
              i <- pInteger
              return $ Nop (s * i)

parseJmp :: Parser Operation
parseJmp = do pString "jmp "
              s <- parseSign
              i <- pInteger
              return $ Jmp (s * i)

parseInstruction :: Parser Operation
parseInstruction = foldr (<|>) (return (Nop 0))
  [parseAcc, parseNop, parseJmp]

eval :: Computer -> Computer
eval (Computer iar ram text visited Running)
  | iar `elem` visited = Computer iar ram text visited Looped
  | succ iar > length text  = Computer iar ram text visited OutOfBounds
  | otherwise = case (text !! iar) of
      Nop _ -> eval $ Computer (succ iar) ram text (iar:visited) Running
      Acc n -> eval $ Computer (succ iar) (ram + n) text (iar:visited) Running
      Jmp n -> eval $ Computer (iar + n) ram text (iar:visited) Running

readData :: String -> [Operation]
readData = catMaybes . map (parse parseInstruction) . lines

initComp :: [Operation] -> Computer
initComp text = Computer 0 0 text [] Running

main = do sample <- readData <$> readFile "data/sample8"
          print $ ram $ eval $ initComp sample
          print $ findStopped $ map (eval . initComp) $ variations sample
          input <- readData <$> readFile "data/input8"
          print $ ram $ eval $ initComp input
          print $ findStopped $ map (eval . initComp) $ variations input

swapInstruction :: Int -> [Operation] -> Maybe [Operation]
swapInstruction 0 (Jmp n:ops) = Just $ Nop n:ops
swapInstruction 0 (Nop n:ops) = Just $ Jmp n:ops
swapInstruction 0 (Acc n:ops) = Nothing
swapInstruction n (op:ops) = (op :) <$> swapInstruction (pred n) ops

variations :: [Operation] -> [[Operation]]
variations ops = catMaybes $ map (flip swapInstruction ops) [0..length ops]

findStopped :: [Computer] -> Maybe Int
findStopped (Computer _ _ _ _ Looped:cs) = findStopped cs
findStopped (Computer _ ram _ _ OutOfBounds:cs) = Just ram
findStopped [] = Nothing

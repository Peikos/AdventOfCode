{-# LANGUAGE OverloadedStrings #-}

module Computer where

import Control.Comonad.Store (Store, store, runStore, peek, seek, seeks, experiment)
import Control.Monad.RWS.Lazy (RWS, execRWS, tell)

import Fourth (digits)

--Types

type Program = Store Int (Maybe Int)
type Address = Int
type Value = Int
type Digit = Int
type OpCode = Int
type AluInst = Value -> Value -> Value
type Pred = Value -> Bool
type Comp = Value -> Value -> Bool
type IntCode = Either Value Address 
type Environment = NonEmpty (Maybe Int)
type Computer = RWS Environment [Maybe Value] Program
type Instruction = (OpCode, [IntCode])
type Action = Maybe (Computer ())
type Operation = Instruction -> Action
type Memory = [Int]

-- Programs

toStore :: [Maybe Value] -> Program
toStore = flip store 0 . (\xs -> join . atIdx xs)

memDump :: Program -> Memory
memDump = catMaybes . takeWhile isJust . experiment (const [0..])

write :: Address -> Maybe Value -> Program -> Program
write key val st = store newLookup curPos
    where (oldLookup, curPos) = runStore st
          newLookup :: Address -> Maybe Value
          newLookup key' | key' == key = val
                         | otherwise   = oldLookup key'

-- Fetching and decoding instructions

fetch :: Program -> [Maybe Value]
fetch = experiment enumFrom

limitTo :: OpCode -> [IntCode] -> [IntCode]
limitTo opcode = take numArgs
  where numArgs | opcode `elem` [3,4]     = 1
                | opcode `elem` [5,6]     = 2
                | opcode `elem` [1,2,7,8] = 3
                | otherwise               = 0

makeIntCode :: Digit -> Maybe Digit -> IntCode
makeIntCode _ Nothing = error "Unreadable IntCode"
makeIntCode mode (Just arg) | mode == 0 = Right arg
                            | otherwise = Left arg

decode :: [Maybe Value] -> Maybe Instruction
decode [] = Nothing
decode vs = do digs <- reverse . digits <$> join (viaNonEmpty head vs)
               args <- viaNonEmpty tail vs
               let opcode = sum . zipWith (*) (iterate (*10) 1) . take 2 $ digs
               let modes = drop 2 digs <> repeat 0
               return (opcode, limitTo opcode $ zipWith makeIntCode modes args)

-- Operation Building Blocks

dereference :: IntCode -> Computer (Maybe Value)
dereference = flip (flip peek >>> either Just) >>> gets

increasePC :: Int -> Computer ()
increasePC = (+) >>> seeks >>> modify

setPC :: Address -> Computer ()
setPC = seek >>> modify

output :: Maybe Value -> Computer ()
output = singleton >>> tell

poke :: Address -> Maybe Value -> Computer ()
poke a = write a >>> modify

try :: Action -> Computer ()
try = fromMaybe nop

popInput :: Computer (Maybe Value) -- ToDo
popInput = do h <- asks head
              local (tail >>> toStream) $ return h

nop :: Computer ()
nop = return ()

alu :: AluInst -> IntCode -> IntCode -> Address -> Action
alu f a b r = Just $ do av <- dereference a
                        bv <- dereference b
                        poke r (f <$> av <*> bv)
                        increasePC 4

jmpc :: Pred -> IntCode -> IntCode -> Action
jmpc p a r = Just $ do av <- mmap p <$> dereference a
                       rv <- mmap setPC <$> dereference r
                       try $ bool (increasePC 3) <$> rv <*> av

cmp :: Comp -> IntCode -> IntCode -> Address -> Action
cmp c a b r = Just $ do av <- dereference a
                        bv <- dereference b
                        poke r $ bool 0 1 <$> (c <$> av <*> bv)
                        increasePC 4

-- Operations

oHalt, oAdd, oMul, oRd, oWrt, oJT, oJF, oEq, oLT :: Operation

oHalt (99, _) = Just $ return ()
oHalt _ = Nothing

oAdd (1, [a, b, Right r]) = alu (+) a b r
oAdd _ = Nothing

oMul (2, [a, b, Right r]) = alu (*) a b r
oMul _ = Nothing

oRd (3, [Right a]) = Just $ popInput >>= poke a >> increasePC 2
oRd _ = Nothing

oWrt (4, [a]) = Just $ dereference a >>= output >> increasePC 2
oWrt _ = Nothing

oJT (5, [a, r]) = jmpc (/= 0) a r
oJT _ = Nothing

oJF (6, [a, r]) = jmpc (== 0) a r
oJF _ = Nothing

oLT (7, [a, b, Right r]) = cmp (<) a b r
oLT _ = Nothing

oEq (8, [a, b, Right r]) = cmp (==) a b r
oEq _ = Nothing

instructions :: [Operation]
instructions = [oAdd, oMul, oRd, oWrt, oJT, oJF, oLT, oEq]

-- Computer

computer :: Computer ()
computer = do op <- gets (fetch >>> decode)
              case mapMaybe (op >>=) instructions of
                  (c:_) -> c >> computer
                  [] -> if isJust $ op >>= oHalt
                          then return ()
                          else error $ "Invalid operation: " <> show op

eval :: [Value] -> Program -> (Program, [Maybe Value])
eval = execRWS computer . toStream . map Just

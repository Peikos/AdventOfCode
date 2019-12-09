{-# LANGUAGE OverloadedStrings #-}

module Computer where

import Control.Comonad.Store ( Store, store, runStore, peek, seek, seeks
                             , experiment)
import Control.Monad.RWS.Lazy (RWS, execRWS, tell)

import Fourth (digits)

--Types

type Memory = Store Int (Maybe Int)
type Address = Int
type Value = Int
type Digit = Int
type OpCode = Int
type AluInst = Value -> Value -> Value
type Pred = Value -> Bool
type Comp = Value -> Value -> Bool
data Argument = Immediate Value | Position Address | Relative Address
       deriving Show
type Computer = RWS () [Maybe Value] ComputerState
type Instruction = (OpCode, [Argument])
type Action = Maybe (Computer ())
type Operation = Instruction -> Action
type StateResult = (ComputerState, [Maybe Value])
data ComputerState = CS { cHalted :: Bool
                        , cRelBase :: Maybe Int
                        , cInput :: [Value]
                        , cMemory :: Memory
                        }

-- Programs

toCS :: [Maybe Value] -> ComputerState
toCS = CS False (Just 0) [] . flip store 0 . (\xs -> join . atIdx xs)

adjRelBase :: Maybe Int -> ComputerState -> ComputerState
adjRelBase b cs = cs { cRelBase = maybePlus b $ cRelBase cs }

halt :: ComputerState -> ComputerState
halt cs = cs { cHalted = True }

pushInput :: [Int] -> ComputerState -> ComputerState
pushInput i cs = cs { cInput = cInput cs ++ i }

loadProgram :: [Int] -> ComputerState
loadProgram = toCS . map Just

memOp :: (Memory -> Memory) -> ComputerState -> ComputerState
memOp f cs = cs { cMemory = f (cMemory cs) }

stackOp :: ([Value] -> [Value]) -> ComputerState -> ComputerState
stackOp f cs = cs { cInput = f (cInput cs) }

write :: Address -> Maybe Value -> Memory -> Memory
write key val st = store newLookup curPos
    where (oldLookup, curPos) = runStore st
          newLookup :: Address -> Maybe Value
          newLookup key' | key' == key = val
                         | otherwise   = oldLookup key'

-- Fetching and decoding instructions

fetch :: Memory -> [Maybe Value]
fetch = experiment enumFrom

limitTo :: OpCode -> [Argument] -> [Argument]
limitTo opcode = take numArgs
  where numArgs | opcode `elem` [3,4,9]   = 1
                | opcode `elem` [5,6]     = 2
                | opcode `elem` [1,2,7,8] = 3
                | otherwise               = 0

makeIntCode :: Digit -> Maybe Digit -> Argument
makeIntCode _ Nothing = error "Unreadable Argument"
makeIntCode mode (Just arg) | mode == 0 = Position arg
                            | mode == 1 = Immediate arg
                            | mode == 2 = Relative arg
                            | otherwise = error "Unknown argument mode"

decode :: [Maybe Value] -> Maybe Instruction
decode [] = Nothing
decode vs = do digs <- reverse . digits <$> join (viaNonEmpty head vs)
               args <- viaNonEmpty tail vs
               let opcode = sum . zipWith (*) (iterate (*10) 1) . take 2 $ digs
               let modes = drop 2 digs <> repeat 0
               return (opcode, limitTo opcode $ zipWith makeIntCode modes args)

-- Operation Building Blocks

memRd :: Address -> Computer (Maybe Value)
memRd adr = gets (peek adr . cMemory)

absolute :: Argument -> Computer Address
absolute (Position p) = return p
absolute (Relative r) = (+r) . fromMaybe 0 <$> gets cRelBase
absolute (Immediate _) = error "toPosition: Immediate"

dereference :: Argument -> Computer (Maybe Value)
dereference (Immediate arg) = return (Just arg)
dereference adr = absolute adr >>= memRd

increasePC :: Int -> Computer ()
increasePC = (+) >>> seeks >>> memOp >>> modify

setPC :: Address -> Computer ()
setPC = seek >>> memOp >>> modify

output :: Maybe Value -> Computer ()
output = singleton >>> tell

poke :: Address -> Maybe Value -> Computer ()
poke a = write a >>> memOp >>> modify

try :: Action -> Computer ()
try = fromMaybe nop

popInput :: Computer (Maybe Value)
popInput = do i <- gets (uncons . cInput)
              case i of
                Just (h, t) -> modify (\c -> c { cInput = t })
                            >> return (Just h)
                Nothing -> return Nothing

nop :: Computer ()
nop = return ()

alu :: AluInst -> Argument -> Argument -> Argument -> Action
alu f a b r = Just $ do av <- dereference a
                        bv <- dereference b
                        rv <- absolute r
                        poke rv (f <$> av <*> bv)
                        increasePC 4

jmpc :: Pred -> Argument -> Argument -> Action
jmpc p a r = Just $ do av <- mmap p <$> dereference a
                       rv <- mmap setPC <$> dereference r
                       try $ bool (increasePC 3) <$> rv <*> av

cmp :: Comp -> Argument -> Argument -> Argument -> Action
cmp c a b r = Just $ do av <- dereference a
                        bv <- dereference b
                        rv <- absolute r
                        poke rv $ bool 0 1 <$> (c <$> av <*> bv)
                        increasePC 4

-- Operations

oHalt, oAdd, oMul, oRd, oWrt, oJT, oJF, oEq, oLT, oBs :: Operation

oHalt (99, _) = Just $ return ()
oHalt _ = Nothing

oAdd (1, [a, b, r]) = alu (+) a b r
oAdd _ = Nothing

oMul (2, [a, b, r]) = alu (*) a b r
oMul _ = Nothing

oRd (3, [a]) = Just $ bind2 poke (absolute a) popInput >> increasePC 2
oRd _ = Nothing

oWrt (4, [a]) = Just $ dereference a >>= output >> increasePC 2
oWrt _ = Nothing

oJT (5, [a, r]) = jmpc (/= 0) a r
oJT _ = Nothing

oJF (6, [a, r]) = jmpc (== 0) a r
oJF _ = Nothing

oLT (7, [a, b, r]) = cmp (<) a b r
oLT _ = Nothing

oEq (8, [a, b, r]) = cmp (==) a b r
oEq _ = Nothing

oBs (9, [a]) = Just $ dereference a >>= modify . adjRelBase >> increasePC 2
oBs _ = Nothing

instructions :: [Operation]
instructions = [oAdd, oMul, oRd, oWrt, oJT, oJF, oLT, oEq, oBs]

-- Computer

computer :: Text -> Computer ()
computer s = do op <- gets (cMemory >>> fetch >>> decode)
                case mapMaybe (op >>=) instructions of
                    (c:_) -> if isJust $ op >>= oWrt
                               then c
                               else c >> computer s
                    [] -> if isJust $ op >>= oHalt
                            then modify halt
                            else error $ "Invalid operation: " <> show op

nonStop :: Text -> Computer ()
nonStop s = ifState cHalted
              (return ())
              (computer s >> nonStop s)

eval :: [Value] -> ComputerState -> StateResult
eval = eval' (computer "")

eval' :: Computer () -> [Value] -> ComputerState -> StateResult
eval' c i = execRWS (modify (pushInput i) >> c) ()

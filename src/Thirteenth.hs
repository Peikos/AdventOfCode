{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}

module Thirteenth (d13p1, d13p2, cheat, demo) where

import Data.Text (splitOn)
import WStore

import Computer
import Lens.Micro (set, over)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Control.Monad.Identity ()

type Screen = WStore (Int, Int) Tile
data Tile = Empty | Wall | Block | Paddle | Ball
  deriving (Show, Eq, Enum, Bounded, Ord)

data Game = Game { _gScreen :: Screen
                 , _gScore :: Int
                 , _gBallPos :: Int
                 , _gPaddlePos :: Int
                 }

makeLenses ''Game

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input13"

newGame :: Game
newGame = Game (wStore' (0,0)) 0 20 22

drawSquare :: Int -> Int -> Int -> Game -> Game
drawSquare (-1) 0 p = over gScore . const $ p
drawSquare x y t = updateGS . (over gScreen . wWrite (x,y) . Just . toEnum $ t)
  where updateGS :: Game -> Game
        updateGS | t == 4    = set gBallPos x 
                 | t == 3    = set gPaddlePos x
                 | otherwise = id

viewRD :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
viewRD (w,h) (x,y) = [(x',y') | y' <- [y..y+h-1], x' <- [x..x+w-1]]

showTile :: Maybe Tile -> Text
showTile (Just Empty) = " "
showTile (Just Wall) = "\ESC[47m \ESC[0m"
showTile (Just Block) = "\ESC[46m \ESC[0m"
showTile (Just Paddle) = "\ESC[38:5:240m_\ESC[0m"
showTile (Just Ball) = "\ESC[1;38mo\ESC[0m"
showTile Nothing = "\ESC[41m \ESC[0m"

render :: Game -> Text
render g = "\ESC[H" <> screen (view gScreen g)
        <> "Current score: " <> show (view gScore g)

screen :: Screen -> Text
screen = unlines . map mconcat . mapp showTile . chunks 44
       . wExperiment (viewRD (44,21)) . wSeek (0,0)

blocks :: Screen -> Int
blocks = length . filter (== Just Block) . wExperiment (viewRD (44,21))
       . wSeek (0,0)

insertQuarter :: ComputerState -> ComputerState
insertQuarter cs = cs { cMemory = wWrite 0 (Just 2) (cMemory cs) }

enlargePanel :: ComputerState -> ComputerState
enlargePanel cs = cs { cMemory = tamper (cMemory cs) }
  where tamper :: Memory -> Memory
        tamper = applyAll $ map (flip wWrite $ Just 3) [1476..1517]

chooseMove :: Game -> Int
chooseMove g = pred . fromEnum $ on compare (flip view g) gBallPos gPaddlePos

mplay :: Monad m => (Game -> m ()) -> (Game -> Int) -> [Int] -> Game
      -> ComputerState -> m (Game, ComputerState)
mplay rf f cm g c = case runSteps 3 "Game" cm c of 
                        (c', [Just x, Just y, Just t])
                            -> rf g >> mplay rf f [] (drawSquare x y t g) c'
                        (c', []) -> if cHalted c'
                                       then rf g >> return (g, c')
                                       else mplay rf f [f g] g c'
                        _ -> error "Invalid response"

play :: (Game -> Int) -> [Int] -> Game -> ComputerState -> (Game, ComputerState)
play f cm g c = runIdentity $ mplay (const $ Identity ()) f cm g c

ioplay :: (Game -> Int) -> [Int] -> Game -> ComputerState -> IO (Game, ComputerState)
ioplay = mplay (putTextLn . render)

demo, cheat, d13p1, d13p2 :: IO Int
cheat = view gScore . fst . play (const 0) [] newGame . enlargePanel
      . insertQuarter <$> input

demo = do putTextLn "\ESC[2J\ESC[;H"
          i <- insertQuarter <$> input
          r <- ioplay chooseMove [] newGame i
          putTextLn ""
          return . view gScore . fst $ r

d13p1 = blocks . view gScreen . fst . play (const 0) [] newGame <$> input
d13p2 = view gScore . fst . play chooseMove [] newGame . insertQuarter <$> input

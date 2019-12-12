{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Eleventh (d11p1, d11p2, printPainting) where

import Control.Comonad.Store ( Store, store, runStore, seek, seeks, experiment
                             , extract)
import Data.Text (splitOn)

import Computer

type Hull = Store (Int, Int) (Maybe Bool)
data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)
type PaintState = (Int, Hull, Direction)

input :: IO ComputerState
input = toCS . map (readMaybe . toString) . splitOn ","
    <$> readFileText "input11"

emptyHull, startWhite :: Hull
emptyHull = store (const Nothing) (0,0)
startWhite = paint True emptyHull

paint :: forall a b. Eq a => b -> Store a (Maybe b) -> Store a (Maybe b)
paint b st = store newLookup curPos
    where (oldLookup, curPos) = runStore st
          newLookup :: a -> Maybe b
          newLookup key | key == curPos = Just b
                        | otherwise     = oldLookup key

view :: Int -> (Int, Int) -> [(Int, Int)]
view r (x,y) = [(x',y') | y' <- [y-r..y+r], x' <- [x-r..x+r]]

turn :: Int -> Direction -> Direction
turn 0 = toEnum . flip mod 4 . pred . fromEnum
turn 1 = toEnum . flip mod 4 . succ . fromEnum
turn _ = id

move :: Direction -> Hull -> Hull
move N = seeks (first succ)
move E = seeks (second succ)
move S = seeks (first pred)
move W = seeks (second pred)

doLoop :: PaintState -> ComputerState -> (PaintState, ComputerState)
doLoop (i, h, d) c =
  case runSteps 2 "PaintBot" [bool 0 1 $ fromMaybe False $ extract h] c of
      (cs', [Just p, Just r]) -> doLoop ( bool (succ i) i $ isJust $ extract h
                                        , move (turn r d) . paint (toEnum p) $ h
                                        , turn r d
                                        ) cs'
      (cs', [])               -> ((i, h, d), cs')
      _                       -> error "Crashing and burning..."

printPainting :: Int -> Hull -> IO ()
printPainting r = putTextLn . unlines . reverse . map mconcat . mapp showPixel
                . filter interesting . transpose . filter interesting
                . chunks (2*r+1) . experiment (view r) . seek (0,0)
  where interesting :: [Maybe Bool] -> Bool
        interesting = any isJust

showPixel :: Maybe Bool -> Text
showPixel (Just True) = "\ESC[47m \ESC[0m"
showPixel _ = " "

d11p1 :: IO Int
d11p1 = fst3 . fst . doLoop (0, emptyHull, N) <$> input

d11p2 :: IO Hull
d11p2 = snd3 . fst . doLoop (0, startWhite, N) <$> input

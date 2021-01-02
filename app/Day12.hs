{-# LANGUAGE TypeApplications, TemplateHaskell #-}

module Day12 where

import Util
import Data.Bifunctor
import Data.Complex
import Data.List
import Data.Maybe
import Lens.Micro.Platform
import Lens.Micro.TH

data Direction = N | S | E | W | L | R | F deriving (Eq, Read, Show)

data Ship = Ship { _pos :: Complex Float
                 , _heading :: Complex Float
                 } deriving Show

makeLenses ''Ship

readData :: String -> [(Direction, Float)]
readData = map readLine . lines
  where readLine :: String -> (Direction, Float)
        readLine = bimap (read @Direction) (read @Float) . splitAt 1

manhattan :: Ship -> Int
manhattan (Ship s _) = round $ abs (realPart s) + abs (imagPart s)

rotate :: Float -> Complex Float -> Complex Float
rotate degrees begin = begin * rot degrees
  where rot 0 = east
        rot 90 = north
        rot 180 = west
        rot 270 = south

north, south, east, west :: Complex Float
north = 0 :+ 1
south = negate north
east = 1
west = negate east

scale :: Float -> Complex Float -> Complex Float
scale scalar comp = (scalar :+ 0) * comp

go :: Float -> Complex Float -> Complex Float -> Complex Float
go distance direction begin = begin + scale distance direction

applyDir :: Ship -> (Direction, Float) -> Ship
applyDir s (N, i) = over pos (go i north) s
applyDir s (S, i) = over pos (go i south) s
applyDir s (E, i) = over pos (go i east) s
applyDir s (W, i) = over pos (go i west) s
applyDir s (L, i) = over heading (rotate i) s
applyDir s (R, i) = over heading (rotate (360-i)) s
applyDir s (F, i) = over pos (go i $ view heading s) s

applyDir2 :: Ship -> (Direction, Float) -> Ship
applyDir2 s (N, i) = over heading (go i north) s
applyDir2 s (S, i) = over heading (go i south) s
applyDir2 s (E, i) = over heading (go i east) s
applyDir2 s (W, i) = over heading (go i west) s
applyDir2 s inst = applyDir s inst

challenge1 = manhattan . foldl applyDir (Ship 0 1)
challenge2 = manhattan . foldl applyDir2 (Ship 0 (10 :+ 1))

main = do sample <- readData <$> readFile "data/sample12"
          print $ challenge1 sample
          print $ challenge2 sample
          input <- readData <$> readFile "data/input12"
          print $ challenge1 input
          print $ challenge2 input

{-# LANGUAGE TypeApplications #-}

module Day11 where

import Util
import Data.Array.Repa hiding (map)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import Data.Array.Repa.Repr.Vector (V)
import Data.Maybe
import Data.Functor.Identity

data Chair = Empty | Taken | Ground deriving (Eq, Show)

type FloorPlan = Array V DIM2 Chair

readData :: String -> FloorPlan
readData input = R.fromList (Z :. length chairs :. length (head chairs)) $ concat chairs
  where chairs = mapp readChair . lines $ input
        readChair 'L' = Empty
        readChair '#' = Taken
        readChair '.' = Ground

chebs :: [(Int, Int)]
chebs = [ (dy, dx)                  -- Generate all chebyshev unit vectors
        | dy <- [-1..1]             -- given both x and y displacement being
        , dx <- [-1..1]             -- either -1, 1 or 0, given that
        , not (dy == 0 && dx == 0)  -- both can't be zero at the same time
        ]

neighbours :: Int -> Int -> FloorPlan -> DIM2 -> [Chair]
neighbours maxY maxX a (Z :. y :. x) =
  [ a ! (Z :. y + dy :. x + dx)     -- Get chair for each neighbour location
  | (dy, dx) <- chebs               -- 8 neighbours surround the current cell with length 1
  , within (0, pred maxY) (y + dy)  -- keep y within bounds
  , within (0, pred maxX) (x + dx)  -- keep x within bounds
  ]

lineOfSight :: Int -> Int -> FloorPlan -> DIM2 -> [Chair]
lineOfSight maxY maxX a (Z :. y :. x) =
  [ fromMaybe Empty . listToMaybe . dropWhile (== Ground) $ -- Get first chair in each ray

    [ a ! (Z :. y + dy * s :. x + dx * s)                       -- Create list of rays as scalar multiples
                                                                --   of cheb unit vectors, translated
    | s <- [1..maximum [maxY - y, y, maxX - x, x] ]             -- no laziness, so limit ray length
    , within (0, pred maxY) (y + s * dy)                        -- keep y within bounds
    , within (0, pred maxX) (x + s * dx)                        -- keep x within bounds
    ]
  | (dy, dx) <- chebs                                       -- generate list of rays for each dir
  ]

step :: (Int -> Int -> FloorPlan -> DIM2 -> [Chair]) -> Int -> FloorPlan -> FloorPlan
step scope threshold arr = runIdentity . computeP . R.traverse arr id $ kernel
  where (Z :. maxY :. maxX) = extent arr
        kernel :: (DIM2 -> Chair) -> DIM2 -> Chair
        kernel _ coord | arr ! coord == Taken    -- Taken chairs become free on overcrowding
                         && (count (== Taken) $ scope maxY maxX arr coord) >= threshold = Empty
                       | arr ! coord == Empty    -- Free chairs get taken if connected chairs are empty
                         && (count (== Taken) $ scope maxY maxX arr coord) == 0 = Taken
                       | otherwise = arr ! coord -- All other chairs stay the same

challenge1 = count (== Taken) . toList . runUntilFix (step neighbours 4)
challenge2 = count (== Taken) . toList . runUntilFix (step lineOfSight 5)

main = do sample <- readData <$> readFile "data/sample11"
          print $ challenge1 sample
          print $ challenge2 sample
          input <- readData <$> readFile "data/input11"
          print $ challenge1 input
          print $ challenge2 input

{-# LANGUAGE OverloadedStrings #-}

module Third (d3p1, d3p2, d3t0, d3t1, d3t2) where

import Data.List (intersect)
import Data.Text (splitOn)
import Relude.Extra.Foldable1 (foldl1')

data Direction = U | D | R | L deriving (Show, Read, Eq)
type WirePath = [(Direction, Int)]
type Coords = [(Int, Int)]

input :: IO (Maybe [WirePath])
input = traverse readWirePath . lines <$> readFileText "input3"

readWirePath :: Text -> Maybe WirePath
readWirePath = traverse (readWirePath' . toString) . splitOn ","
  where readWirePath' :: String -> Maybe (Direction, Int)
        readWirePath' [] = Nothing
        readWirePath' (d:l) = do dir <- readMaybe [d]
                                 len <- readMaybe l
                                 return (dir, len)

origin :: Coords
origin = singleton (0, 0)

wirePathToCoords :: WirePath -> Maybe Coords
wirePathToCoords = wirePathToCoords' origin
  where wirePathToCoords' :: Coords -> WirePath -> Maybe Coords
        wirePathToCoords' ((x,y):xys) ((U, dy):ps)
            = wirePathToCoords' ([(x, ny) | ny <- [y+dy, y+dy-1..y]] ++ xys) ps
        wirePathToCoords' ((x,y):xys) ((D, dy):ps)
            = wirePathToCoords' ([(x, ny) | ny <- [y-dy..y]] ++ xys) ps
        wirePathToCoords' ((x,y):xys) ((R, dx):ps)
            = wirePathToCoords' ([(nx, y) | nx <- [x+dx, x+dx-1..x]] ++ xys) ps
        wirePathToCoords' ((x,y):xys) ((L, dx):ps)
            = wirePathToCoords' ([(nx, y) | nx <- [x-dx..x]] ++ xys) ps
        wirePathToCoords' ps [] = Just ps
        wirePathToCoords' [] _ = Nothing

manhattan :: (Int, Int) -> Int
manhattan = uncurry (+) . bimapBoth abs

closest :: [Int] -> Maybe Int
closest = flip atIdx 1 . sort

intersects :: [Coords] -> Maybe Coords
intersects = viaNonEmpty (foldl1' intersect)

closestIntersect :: Maybe [WirePath] -> Maybe Int
closestIntersect w = w >>= traverse wirePathToCoords >>= intersects
                       >>= closest . map manhattan

shortestPath :: Maybe [WirePath] -> Maybe Int
shortestPath mw = do w <- mw
                     coords <- traverse wirePathToCoords w
                     let [fa, fb] = map (indexOf . reverse) coords
                     ints <- intersects coords
                     lens <- traverse (\int -> (+) <$> fa int <*> fb int) ints
                     closest lens

d3t0, d3t1, d3t2 :: Maybe [WirePath]
d3t0 = traverse readWirePath ["R8,U5,L5,D3"
                             ,"U7,R6,D4,L4"]
d3t1 = traverse readWirePath ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                             ,"U62,R66,U55,R34,D71,R55,D58,R83"]
d3t2 = traverse readWirePath ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                             ,"U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]

d3p1, d3p2 :: IO (Maybe Int)
d3p1 = closestIntersect <$> input
d3p2 = shortestPath <$> input

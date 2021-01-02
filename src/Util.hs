module Util where

import Data.Bool
import Data.Maybe

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

within :: Ord a => (a, a) -> a -> Bool
within (min, max) val = val >= min && val <= max

result :: Show s => IO s -> IO ()
result = (>>= print)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)

digit = flip elem "0123456789"
hex = flip elem "0123456789abcdef"

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id 

countTrue f = sum . map (bool 0 1 . f)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f = zipWith f <*> tail

filterMask :: [Bool] -> [a] -> [a]
filterMask mask = catMaybes . zipWith (\b e -> if b then Just e else Nothing) mask

maskWith :: ([a] -> [Bool]) -> [a] -> [a]
maskWith p = flip filterMask <*> p

when :: (a -> Bool) -> a -> Maybe a
when p a | p a = Just a
         | otherwise = Nothing

elemOrd :: Ord a => a -> [a] -> Bool
elemOrd a [] = False
elemOrd a (x:xs) =
        case compare a x of
          GT -> elemOrd a xs
          EQ -> True
          _  -> False

mapp = map . map

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

runUntilFix :: Eq a => (a -> a) -> a -> a
runUntilFix f a | a == f a = a
                | otherwise = runUntilFix f (f a)

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst p = head . dropWhile (not . p)


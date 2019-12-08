{-# LANGUAGE OverloadedStrings #-}

module Eighth  where

type Pixel = Maybe Bool
type Layer = [[Pixel]]

input :: IO [Layer]
input = map (splitLayers 25 . map pixel) <$> rawInput

rawInput :: IO [[Int]]
rawInput = filter (not . null) . map catMaybes . splitLayers (25*6)
         . map (readMaybe . singleton) <$> readFile "input8"

splitLayers :: Int -> [a] -> [[a]]
splitLayers dims = unfoldr f
  where f :: [a] -> Maybe ([a], [a])
        f [] = Nothing
        f ps = Just $ splitAt dims ps

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

pixel :: Int -> Pixel
pixel 0 = Just False
pixel 1 = Just True
pixel 2 = Nothing
pixel _ = error "pixel"

emptyLayer :: Layer
emptyLayer = repeat . repeat $ Nothing

firstAnswer :: IO (Maybe Int)
firstAnswer = fmap (uncurry (*) . (count 1 &&& count 2)) . viaNonEmpty head
            . sortOn (count 0) <$> rawInput

secondAnswer :: IO Layer
secondAnswer = foldr (zippWith mplus) emptyLayer <$> input

showPixel :: Pixel -> Text
showPixel (Just True) = "\ESC[40m \ESC[0m"
showPixel (Just False) = "\ESC[47m \ESC[0m"
showPixel Nothing = "?"

printImage :: Layer -> IO ()
printImage = putTextLn . unlines . map mconcat . mapp showPixel

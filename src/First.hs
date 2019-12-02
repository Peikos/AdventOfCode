module First (firstAnswer, secondAnswer) where

import Data.Monoid ()

input :: IO [Maybe (Sum Int)]
input = map (fmap Sum . readMaybe . toString) . lines <$> readFileText "input1"

fuel :: Sum Int -> Sum Int
fuel =  subtract 2 . fmap (flip div 3)

firstAnswer :: IO (Maybe (Sum Int))
firstAnswer = foldMap (fmap fuel) <$> input

fuel' :: Sum Int -> Sum Int
fuel' = mconcat . takeWhile (> mempty) . iterate fuel . fuel

secondAnswer :: IO (Maybe (Sum Int))
secondAnswer = foldMap (fmap fuel') <$> input

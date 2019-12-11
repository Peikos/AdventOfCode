module First (d1p1, d1p2) where

import Data.Monoid ()

input :: IO [Maybe (Sum Int)]
input = map (fmap Sum . readMaybe . toString) . lines <$> readFileText "input1"

fuel :: Sum Int -> Sum Int
fuel =  subtract 2 . fmap (flip div 3)

d1p1 :: IO (Maybe (Sum Int))
d1p1 = foldMap (fmap fuel) <$> input

fuel' :: Sum Int -> Sum Int
fuel' = mconcat . takeWhile (> mempty) . iterate fuel . fuel

d1p2 :: IO (Maybe (Sum Int))
d1p2 = foldMap (fmap fuel') <$> input

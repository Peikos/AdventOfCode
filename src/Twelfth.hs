module Twelfth (d12p1, d12p2, d12t10, d12t11, d12t20, d12t20') where

data R3 = C { x :: Int, y :: Int, z :: Int } deriving (Eq, Show, Ord)

data Moon = Moon { pos :: R3
                 , vel :: R3
                 } deriving (Eq, Show, Ord)

instance Semigroup R3 where
  (C x1 y1 z1) <> (C x2 y2 z2) = C (x1+x2) (y1+y2) (z1+z2)

instance Monoid R3 where
  mempty = C 0 0 0

input, ts0, ts1 :: [Moon]
input = [ Moon (C (-16) 15 (-9)) mempty
        , Moon (C (-14)  5   4)  mempty
        , Moon (C    2   0   6)  mempty
        , Moon (C  (-3) 18   9)  mempty
        ]

sig :: R3 -> R3 -> R3
sig (C x1 y1 z1) (C x2 y2 z2) = C (cp x1 x2) (cp y1 y2) (cp z1 z2)
  where cp a b = case compare a b of
                      GT -> negate 1
                      LT -> 1
                      EQ -> 0

updatePos :: Moon -> Moon
updatePos (Moon p v) = Moon (p <> v) v

updateVel :: Moon -> [Moon] -> Moon
updateVel (Moon p v) = Moon p . (v <>) . mconcat . map (sig p . pos)

updateVels :: [Moon] -> [Moon]
updateVels ms = map (subst updateVel (remFrom ms)) ms
-- updateVels = subst for (subst updateVel . remFrom)

update :: [Moon] -> [Moon]
update = map updatePos . updateVels

steps :: Int -> [Moon] -> [Moon]
steps n = fromMaybe [] . viaNonEmpty last . take (succ n) . iterate update

absSum :: R3 -> Sum Int
absSum (C a b c) = foldMap (Sum . abs) [a, b, c]

energy :: Moon -> Sum Int
energy (Moon p v) = on (*) absSum p v

totalEnergy :: [Moon] -> Int
totalEnergy = getSum . foldMap energy

naive :: [Moon] -> [Moon] -> Int
naive start cur | start == cur = 1
                | otherwise    = succ $ naive start (update cur)

detectCycle :: Eq a => [a] -> Maybe Int
detectCycle [] = Nothing
detectCycle xs = viaNonEmpty head $ filter f [1..]
  where f i = let fc = take i xs
                  sc = take i . drop i $ xs
              in  sc == fc

stateList :: [Moon] -> [Int]
stateList = concatMap extract
  where extract (Moon (C a b c) (C d e f)) = [a, b, c, d, e, f]

smart :: [Moon] -> Int
smart = foldr lcm 1 . take 24 . mapMaybe detectCycle . transpose . map stateList
      . iterate update

ts0 = [ Moon (C (-1)   0    2)  mempty
      , Moon (C   2 (-10) (-7)) mempty
      , Moon (C   4  (-8)   8)  mempty
      , Moon (C   3    5  (-1)) mempty
      ]

ts1 = [ Moon (C (-8) (-10)   0)  mempty
      , Moon (C   5     5   10)  mempty
      , Moon (C   2   (-7)   3)  mempty
      , Moon (C   9   (-8) (-3)) mempty
      ]

d12p1, d12p2, d12t10, d12t11, d12t20, d12t20' :: Int
d12p1 = totalEnergy $ steps 1000 input
d12p2 = smart input
d12t10 = totalEnergy $ steps 10 ts0
d12t11 = totalEnergy $ steps 100 ts1
d12t20 = subst naive update ts0
d12t20' = smart ts0

module Fourth (d4p1, d4p2, digits) where

imin, imax :: Int
imin = 168630
imax = 718098

digits :: Int -> [Int]
digits = reverse . unfoldr coalg
  where coalg :: Int -> Maybe (Int, Int)
        coalg 0 = Nothing
        coalg i = Just . swap . flip divMod 10 $ i

monotonic :: [Int] -> Bool
monotonic = isJust . foldl' stillNotLower (Just 0)
  where stillNotLower mx y = mx >>= guarded (<y)

digitOccurrence :: (Int -> Bool) -> [Int] -> Bool
digitOccurrence p = any (p . length) . group

findCode :: (Int -> Bool) -> Int
findCode p = length [ s | s <- map digits [imin..imax]
                    , monotonic s, digitOccurrence p s]

d4p1, d4p2 :: Int
d4p1 = findCode (>=2)
d4p2 = findCode (==2)

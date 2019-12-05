module Fourth where

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

firstAnswer, secondAnswer :: Int
firstAnswer = findCode (>=2)
secondAnswer = findCode (==2)

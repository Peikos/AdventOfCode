{-# LANGUAGE OverloadedStrings #-}

module Tenth (d10p1, d10p2, d10t0, d10t1, d10t2, d10t3, d10t4, d10t5) where

import Data.Ratio ()
import Data.List (nub, groupBy)

newtype Asteroid = A { fromA :: (Int, Int) } deriving (Eq, Show)
type AstMap = [Asteroid]
type Answer = (Asteroid, (Maybe Double, Int))

input :: IO AstMap
input = readMap <$> readFileText "input10"

infGrid :: [[(Int, Int)]]
infGrid = [[(x,y) | x <- [0..]] | y <- [0..]]

readMap :: Text -> AstMap
readMap = catMaybes . concat . zippWith createAst infGrid . map toString . lines
  where createAst :: (Int, Int) -> Char -> Maybe Asteroid
        createAst c '#' = Just (A c)
        createAst _ _ = Nothing

instance Num Asteroid where
  (A (x1, y1)) + (A (x2, y2)) = A (x1+x2, y1+y2)
  (A (x1, y1)) - (A (x2, y2)) = A (x1-x2, y1-y2)
  _            * _            = error "Don't. Multiply. Asteroids."
  abs (A (x, y)) = A (abs x, abs y)
  signum (A (x, y)) = A (signum x, signum y)
  fromInteger x = A (fromInteger x, 0)

relativeTo :: Asteroid -> AstMap -> AstMap
relativeTo o = map (subtract o)

detects :: AstMap -> Int
detects = pred . length . nub . map phase

findBest :: AstMap -> Maybe (Asteroid, Int)
findBest i = viaNonEmpty last . sortOn snd
           . map (id &&& detects . flip relativeTo i)
           $ i

decimalPart :: Double -> Double
decimalPart x = x - fromIntegral (truncate x :: Int)

phase :: Asteroid -> Maybe Double
phase (A (x, y)) | x == 0 && y == 0 = Nothing
                 | x == 0 && y > 0  = Just 0.5
                 | x == 0 && y < 0  = Just 0
                 | x > 0            = Just $ decimalPart $ 0.25 + atan
                          (fromIntegral y / fromIntegral x) / (2 * pi)
                 | x < 0            = Just $ 0.75 + atan
                          (fromIntegral y / fromIntegral x) / (2 * pi)
                 | otherwise        = error "phase: invalid asteroid"

distSq :: Asteroid -> Int
distSq (A (x, y)) = x^(2 :: Int) + y^(2 :: Int)

label :: (Int, Int) -> [Asteroid] -> [Answer]
label o = filter (/= (A o, (Nothing, 0))) -- remove laser location
        . map (((+ A o) &&& (phase &&& distSq)) . subtract (A o))

findOrder :: [Answer] -> [Answer]
findOrder = concat . transpose . map (sortOn (snd . snd))
          . groupBy ((==) `on` (fst . snd)) . sortOn (fst . snd)

formatAnswer :: Answer -> Int
formatAnswer = uncurry (+) . first (*100) . fromA . fst

d10p1, d10p2 :: IO (Maybe Int)
d10p1 = fmap snd . findBest <$> input
d10p2 = fmap formatAnswer . flip atIdx 199 . findOrder
             . label (20,19) <$> input

d10t0, d10t1, d10t2, d10t3, d10t4, d10t5 :: AstMap
d10t0 = readMap ".#..#\n.....\n#####\n....#\n...##"
d10t1 = readMap $ "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n"
               <> "..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
d10t2 = readMap $ "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n"
               <> ".##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
d10t3 = readMap $ ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n"
               <> "....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
d10t4 = readMap $ ".#..##.###...#######\n##.############..##.\n" -- laser 11,13
               <> ".#.######.########.#\n.###.#######.####.#.\n"
               <> "#####.##.#.##.###.##\n..#####..#.#########\n"
               <> "####################\n#.####....###.#.#.##\n"
               <> "##.#################\n#####.##.###..####..\n"
               <> "..######..##.#######\n####.##.####...##..#\n"
               <> ".#####..#.######.###\n##...#.##########...\n"
               <> "#.##########.#######\n.####.#.###.###.#.##\n"
               <> "....##.##.###..#####\n.#.#.###########.###\n"
               <> "#.#.#.#####.####.###\n###.##.####.##.#..##"
d10t5 = readMap $ ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n"
               <> "..#.....X...###..\n..#.#.....#....##" -- laser 8,3

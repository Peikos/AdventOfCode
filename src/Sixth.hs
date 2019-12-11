{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveFunctor #-}

module Sixth (d6p1, d6p2, d6t0, d6t1) where

import Data.Text (splitOn)
import Data.List ((\\), nub, groupBy)
import Relude.Extra.Map (lookup, keys, elems)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Foldable (Fix(Fix), unfix, cata, ana)

-- Types

type OrbitMap = Map TreeItem [TreeItem]

data TreeItem = Empty
              | Santa
              | You
              | Label Text
              | Number Int
              | Distances (Maybe Int) (Maybe Int)
  deriving (Show, Eq, Ord)

data TreeF v a = Node { val :: v, children :: [a] } deriving (Show, Eq, Functor)
$(deriveShow1 ''TreeF)
type Tree v = Fix (TreeF v)

type OrbitTree = Tree TreeItem

-- Handling puzzle input

input :: IO OrbitMap
input = readMap <$> readFileText "input6"

-- Algebra and recursion schemes

leastAlgebra :: Algebra (TreeF TreeItem) (Maybe Int)
leastAlgebra (Node (Number v) ns) = foldr maybeMin (Just v) ns
leastAlgebra _ = Nothing

sumAlgebra :: Algebra (TreeF TreeItem) Int
sumAlgebra (Node (Number v) ns) = v + sum ns
sumAlgebra _ = 0

buildCoalgebra :: OrbitMap -> Coalgebra (TreeF TreeItem) TreeItem
buildCoalgebra om label = Node label . concat . lookup label $ om

-- Input manipulation

readMap :: Text -> OrbitMap
readMap = fromList . mapMaybe parseOrbits . groupByBody
                   . mapMaybe (uncons . splitOn ")") . lines

groupByBody :: [(Text, [Text])] -> [[(Text, [Text])]]
groupByBody = groupBy ((==) `on` fst) . sortOn fst

parseOrbits :: [(Text, [Text])] -> Maybe (TreeItem, [TreeItem])
parseOrbits = liftMaybeTuple . bimap (fmap Label . viaNonEmpty head)
                                     (map Label . join)
                             . unzip

centre :: OrbitMap -> Maybe TreeItem
centre om = listToMaybe $ nub (keys om) \\ concat (elems om)

-- Tree Manipulation

countLevel :: Int -> OrbitTree -> OrbitTree
countLevel l (Fix (Node (Label _) ns)) =
              Fix . Node (Number l) . map (countLevel (succ l)) $ ns
countLevel _ tree = tree

addDists :: OrbitTree -> OrbitTree
addDists (Fix (Node (Label "SAN") _)) = Fix $ Node Santa []
addDists (Fix (Node (Label "YOU") _)) = Fix $ Node You []
addDists (Fix (Node (Label _) ns)) = let cs = map addDists ns
                                         cd = cDists $ map (val . unfix) cs
                                     in Fix $ Node cd cs
addDists tree = tree

cDists :: [TreeItem] -> TreeItem
cDists cs | Santa `elem` cs        = Distances (Just 0) Nothing
          | You   `elem` cs        = Distances Nothing (Just 0)
          | dSum cs == Empty       = Empty
          | otherwise              = incrementDistances $ dSum cs
  where incrementDistances (Distances s y) = Distances (succ <$> s) (succ <$> y)
        incrementDistances e               = error $ show e

findConnections :: OrbitTree -> OrbitTree
findConnections (Fix (Node (Distances (Just s) (Just y)) ns))
               = Fix (Node (Number (s + y)) ns)
findConnections n = n

dSum :: [TreeItem] -> TreeItem
dSum = foldr (<+>) Empty
  where (<+>) :: TreeItem -> TreeItem -> TreeItem
        (Distances as ay) <+> (Distances bs by)
           = Distances (maybePlus as bs) (maybePlus ay by)
        a <+> Empty = a
        Empty <+> b = b
        _ <+> _ = error "dPlus: no Distances provided"

-- Answers

d6p1, d6p2 :: IO (Maybe Int)
d6p1 = firstPL <$> input
d6p2 = secondPL <$> input

firstPL :: OrbitMap -> Maybe Int
firstPL om = do c <- centre om
                return $ ana (buildCoalgebra om)
                     >>> topDown (countLevel 0)
                     >>> cata sumAlgebra $ c

secondPL :: OrbitMap -> Maybe Int
secondPL om = do c <- centre om
                 ana (buildCoalgebra om)
                  >>> topDown (addDists >>> findConnections)
                  >>> cata leastAlgebra $ c

-- Tests
d6t0, d6t1 :: Maybe Int
d6t0 = firstPL $ readMap
       "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
d6t1 = secondPL $ readMap
       "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveFunctor #-}

module Fourteenth  where

import Data.Attoparsec.Text
import Data.Char (isAlpha)
import Relude.Extra.Map (lookup, keys, elems)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Foldable (Fix(Fix), unfix, cata, ana)

type Label = Text
type Target = (Label, Int)
data Recipe = Recipe { num :: Int
                     , rqs :: [Target]
                     } deriving (Show)

data TreeF v a = Node { val :: v, children :: [a] } deriving (Show, Eq, Functor)
$(deriveShow1 ''TreeF)
type Tree v = Fix (TreeF v)

type CraftTree = Tree (Int, Recipe)

input :: IO (Map Label Recipe)
input = fromList . mapMaybe procLine . lines <$> readFileText "input14"

tinp :: Map Label Recipe
tinp = fromList . mapMaybe procLine $ [ "10 ORE => 10 A"
                                      , "1 ORE => 1 B"
                                      , "7 A, 1 B => 1 C"
                                      , "7 A, 1 C => 1 D"
                                      , "7 A, 1 D => 1 E"
                                      , "7 A, 1 E => 1 FUEL"
                                      ]  -- 7 A = 10 ORE, 1E


buildCoalgebra :: Map Label Recipe -> Coalgebra (TreeF Target) Target
buildCoalgebra rm (l, i) = Node (l, i * maybe 1 num (lookup l rm)) $ rqs (fromMaybe (Recipe i []) $ lookup l rm)
-- * i , rqs

test :: Map Label Recipe -> Fix (TreeF Target)
test inp = ana (buildCoalgebra inp) ("FUEL", 1)

-- Parser related stuff

procLine :: Text -> Maybe (Label, Recipe)
procLine = rightToMaybe . parseOnly (parseLine <* endOfInput)

ignoringSpace :: Parser a -> Parser a
ignoringSpace p = do skipSpace
                     a <- p
                     skipSpace
                     return a

parseLine :: Parser (Label, Recipe)
parseLine = do reqs <- sepBy parseReq (char ',')
               -- skipSpace
               _ <- ignoringSpace $ string "=>"
               -- skipSpace
               n <- ignoringSpace decimal
               -- skipSpace
               l <- ignoringSpace $ takeWhile1 isAlpha
               return (l, Recipe n reqs)

parseReq :: Parser (Label, Int)
parseReq = do --skipSpace
              n <- ignoringSpace decimal
              -- skipSpace
              l <- ignoringSpace $ takeTill (flip elem [' ',',', '='])
              return (l, n)

--procLine = maybeResult . parse parseLine
{- procLine l = case parse parseLine l of
  Fail e _ _ -> error e
  Partial p -> error "P" -- case p "" of
                   -- Fail e ss s -> error $ e <> show (ss,s)
                   -- Done _ r -> Just r
  Done _ r -> Just r -}

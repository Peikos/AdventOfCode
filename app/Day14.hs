module Day14 where

import Parser
import Util

import Control.Applicative
import Data.Bool
import Data.List
import Data.Maybe
import Data.Tuple

import Data.Map (Map)
import qualified Data.Map as Map

data Mask = Mask [Maybe Bool] deriving Show
data Assignment = Ass Int Integer deriving Show
data Section = Sec Mask [Assignment] deriving Show
data Memory = Mem { runMem :: Map Int Integer } deriving Show

--readData :: String -> [Section]
readData = fromJust . parse (pRepeatSepBy pWhitespace parseSection)

parseSection :: Parser Section
parseSection = do mask <- parseMask
                  _ <- pNewline
                  asss <- pRepeatSepBy pNewline parseAssignment
                  return $ Sec mask asss

parseMask :: Parser Mask
parseMask = do _ <- pString "mask = "
               Mask <$> pRepeat parseBit

parseBit :: Parser (Maybe Bool)
parseBit = (const (Just True) <$> pChar '1') <|> (const (Just False) <$> pChar '0') <|> (const Nothing <$> pChar 'X')

parseAssignment :: Parser Assignment
parseAssignment = do _ <- pString "mem["
                     addr <- pInteger
                     _ <- pString "] = "
                     val <- fromIntegral <$> pInteger
                     return $ Ass addr val

toBin :: Integer -> [Maybe Bool]
toBin = map (Just . (== 1)) . take 38 . (++ repeat 0) . unfoldr (when (/= (0,0)) . swap . flip divMod 2)

fromBin :: [Maybe Bool] -> Integer
fromBin = sum . zipWith (\p (Just b) -> p * bool 0 1 b) [2^e | e <- [0..38]]

handleSections :: [Section] -> Memory
handleSections = foldl handleSection (Mem Map.empty)

handleSection :: Memory -> Section -> Memory
handleSection mem (Sec _ []) = mem
handleSection (Mem mem) (Sec (Mask m) ((Ass k v):asss)) =
  handleSection (Mem $ Map.insert k (mask v) mem) (Sec (Mask m) asss)
    where mask = fromBin . zipWith (<|>) (reverse m) . toBin -- Profunctor / lens iso


challenge1 :: [Section] -> Integer
challenge1 = foldr (+) 0 . runMem . handleSections

main = do putStrLn "Sample data"
          sample <- readData <$> readFile "data/sample14"
          print sample
          print $ handleSections sample
          print $ challenge1 sample
          input <- readData <$> readFile "data/input14"
          --print $ handleSections input
          print $ challenge1 input

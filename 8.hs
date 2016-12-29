module Main where

import Control.Applicative ((<$>),(<$))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, runStateT)
import Data.List

import Text.Parsec
import Text.Parsec.Error

type Parser = Parsec String ()

newtype LCD = LCD {lcd :: [[Bool]]}

instance Show LCD where
  show = unlines . map (concatMap (\x -> if x then "[]" else "..")) . lcd

screen :: Int -> Int -> LCD
screen c r = LCD $ replicate r (replicate c False)

parseLCD :: String -> Either ParseError [LCD -> LCD]
parseLCD = runParser pInstructions () ""

execute c r = either (const $ screen c r)
                     (($ screen c r) . foldl1 (flip (.))) . parseLCD

pRect :: Parser (LCD -> LCD)
pRect = do string "rect"
           many1 space
           x <- many1 digit
           char 'x'
           y <- many1 digit
           return $ rect (read x) (read y)

pRRow :: Parser (LCD -> LCD)
pRRow = do string "rotate row y="
           r <- many1 digit
           string " by "
           a <- many1 digit
           return $ rotateR (read r) (read a)

pRCol :: Parser (LCD -> LCD)
pRCol = do string "rotate column x="
           c <- many1 digit
           string " by "
           a <- many1 digit
           return $ rotateC (read c) (read a)

pInstruction = try pRect <|> try pRRow <|> try pRCol <|> (const id <$> eof)
pInstructions = pInstruction `sepBy` char '\n'

rect :: Int -> Int -> LCD -> LCD
rect x y = LCD . zipWith (zipWith (||)) (replicate y (replicate x True ++ repeat False) ++ repeat (repeat False)) . lcd

rotateR :: Int -> Int -> LCD -> LCD
rotateR r a (LCD lcd) = LCD $ take r lcd ++ [f (lcd !! r)] ++ drop (r + 1) lcd
  where f row = drop a' row ++ take a' row
        a' = (length . head $ lcd) - a

rotateC :: Int -> Int -> LCD -> LCD
rotateC c a = LCD . transpose . lcd . rotateR c a . LCD . transpose . lcd

test = unlines ["rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4", "rotate column x=1 by 1"]

main = do input <- readFile "8.txt"
          print $ execute 7 3 test
          print $ execute 50 6 input
          print . sum . map (length . filter id) . lcd . execute 50 6 $ input

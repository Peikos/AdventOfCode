module Main where

import Control.Applicative ((<$>),(<$))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, runStateT)
import Data.List

import Text.Parsec
import Text.Parsec.Error

type Parser = Parsec String ()

countEncoding :: String -> Either ParseError Int
countEncoding = runParser cText () ""

parseEncoding :: String -> Either ParseError String
parseEncoding = runParser pText () ""

cMarker :: Parser Int
cMarker = do char '('
             c <- many1 digit
             char 'x'
             n <- many1 digit
             char ')'
             str <- count (read c) anyChar
             let (Right i) = countEncoding str
             return ((read n) * i)

cChar :: Parser Int
cChar = (\x -> if x `elem` " \n" then 0 else  1) <$> anyChar

cAll :: Parser Int
cAll = try cMarker <|> cChar

cText = sum <$> many1 cAll

pMarker :: Parser String
pMarker = do char '('
             c <- many1 digit
             char 'x'
             n <- many1 digit
             char ')'
             str <- count (read c) anyChar
             return . concat . replicate (read n) $ str

pChar :: Parser String
pChar = (:[]) <$> anyChar

pAll :: Parser String
pAll = try pMarker <|> pChar -- <|> (const [] <$> eof)
pText = concat <$> many1 pAll

main = do input <- readFile "9.txt"
          print . fmap (length . filter (not . flip elem "\n ")) . parseEncoding $ input
          print $ countEncoding input

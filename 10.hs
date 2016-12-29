module Main where

import Control.Applicative ((<$>),(<$))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (StateT, runStateT)
import Data.List
import System.Process

import Text.Parsec
import Text.Parsec.Error

type Parser = Parsec String ()

data Pred = At String String
          | Give String String String
  deriving Show

pAt :: Parser Pred
pAt = do string "value "
         v <- many1 digit
         string " goes to bot "
         b <- many1 digit
         return $ At v b

pGive :: Parser Pred
pGive = do string "bot "
           b <- many1 digit
           string " gives low to "
           l <- pRecip
           string " and high to "
           h <- pRecip
           return $ Give b l h

pRecip = try pBot <|> pBin

pBot :: Parser String
pBot = do string "bot "
          b <- many1 digit
          return b

pBin :: Parser String
pBin = do string "output "
          o <- many1 digit
          return $ 'o' : o

pAll :: Parser Pred
pAll = try pAt <|> try pGive
p = pAll `sepEndBy` char '\n'

prolog :: [Pred] -> String
prolog = unwords . map prolog'
  where prolog' (At v b) = "at(" ++ intercalate "," [v,b] ++  ")."
        prolog' (Give b l h) = "give(" ++ intercalate "," [b,l,h] ++ ")."

main = do input <- readFile "10.txt"
          putStrLn "== Parsing input =="
          case runParser p () "" input of
            (Left err) -> print err
            (Right pl) -> do putStrLn "== Generating Prolog =="
                             putStrLn . prolog $ pl
                             writeFile "10.output.pl" $ header ++ prolog pl
                             answer <- readCreateProcess (shell "swipl7 -s 10.output.pl -g \"solve(17,61,Sol), print(Sol), nl, solve2(Sol2), print(Sol2).\" -t halt") ""
                             putStrLn "== Solutions Found =="
                             putStrLn answer

header = unlines $
  [ ":- dynamic at/2."
  , ":- discontiguous at/2."
  , ":- discontiguous give/3."
  , "hasBoth(R, L, H) :- at(L,R), at(H,R), L < H."
  , "doGive(Robot, A, B, T1, T2) :- at(A, Robot), at(B, Robot), A < B, give(Robot, T1, T2), retract(at(A, Robot)), assert(at(A, T1)), retract(at(B, Robot)), assert(at(B,T2))."
  , "doGiveUntil(Robot, A, B, T1, T2, L, H, Sol) :- doGive(Robot, A, B, T1, T2), \\+hasBoth(Sol,L,H), doGiveUntil(_,_,_,_,_,_,_,_)."
  , "doGiveUntil(_,_,_,_,_,_,_,_)."
  , "doGiveUntilEmpty(Robot, A, B, T1, T2) :- doGive(Robot, A, B, T1, T2), binEmpty, doGiveUntil(_,_,_,_,_,_,_,_)."
  , "solve(L, H, Sol) :- hasBoth(Sol, L, H)."
  , "solve(L, H, Sol) :- doGiveUntil(_, _, _, _, _, L, H, Sol), hasBoth(Sol, L, H)."
  , "solve(L, H, Sol) :- limit(1, solve(L, H, Sol))."
  , "solve2(Sol) :- at(A, o0), at(B, o1), at(C, o2), limit(1,Sol is A*B*C)."
  , "solve2(Sol) :- doGiveUntilEmpty(_,_,_,_,_), limit(1,solve2(Sol))."
  , "binEmpty :- \\+(at(_,o0))."
  , "binEmpty :- \\+(at(_,o1))."
  , "binEmpty :- \\+(at(_,o2))."
  ]

module Parser  where
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Control.Applicative
import Control.Monad
import Data.List (uncons)

type Parser = StateT String Maybe
type CharSet = [Char]

pConditional :: (Char -> Bool) -> Parser Char
pConditional p = do input <- uncons <$> get
                    case input of
                      Just (h, t) -> if p h then put t >> return h else empty
                      Nothing -> empty

pAny :: Parser Char
pAny = pConditional (const True)

pMany :: Parser String
pMany = pRepeat pAny

pChar :: Char -> Parser Char
pChar c = pConditional (== c)

pString :: String -> Parser String
pString [] = return []
pString (c:cs) = do p <- pChar c
                    ps <- pString cs
                    return $ p:ps

pDigit :: Parser Char
pDigit = pConditional (flip elem "1234567890")

pRepeatSepBy' :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy' sep p = pRepeatSepBy sep p <|> return []

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

pRepeat :: Parser b -> Parser [b]
pRepeat = pRepeatSepBy $ return ()

pRepeat' :: Parser b -> Parser [b]
pRepeat' = pRepeatSepBy' $ return ()

pInteger :: Parser Int
pInteger = read <$> pRepeat pDigit

pOneOf :: [String] -> Parser String
pOneOf = foldr (<|>) empty . map pString

pWhitespace = const () <$> (pRepeat' $ pConditional isWhitespace)
  where isWhitespace = flip elem [' ', '\t', '\n']

pNewline = pChar '\n'

trimmed :: Parser a -> Parser a
trimmed p = do _ <- pWhitespace
               a <- p
               _ <- pWhitespace
               return a

parse :: Parser a -> String -> Maybe a
parse = evalStateT

{-# LANGUAGE OverloadedStrings #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , atIdx, indexOf, singleton, bimapBoth, guarded, fromJust, mmap
       , toStream
       ) where

import Relude
import Data.List ((!!))

singleton :: a -> [a]
singleton = return

atIdx :: [a] -> Int -> Maybe a
atIdx xs i | i < length xs = Just $ xs !! i
           | otherwise   = Nothing

bimapBoth :: Bifunctor f => (a -> b) -> f a a -> f b b
bimapBoth f = bimap f f

indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf (x:xs) t | x == t    = Just 0
                 | otherwise = succ <$> indexOf xs t
indexOf [] _ = Nothing

guarded :: Alternative m => (a -> Bool) -> a -> m a
guarded p v = if p v then pure v else empty

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap = fmap

toStream :: [Maybe a] -> NonEmpty (Maybe a)
toStream = fromJust . nonEmpty . (<> repeat Nothing)

{-# LANGUAGE OverloadedStrings #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , atIdx, indexOf, singleton, bimapBoth, guarded, fromJust
       , mmap, mapp, zippWith, zipWith3, chunks, fst3, snd3, thrd3
       , toStream, maybeAny, maybePlus, maybeMin, liftMaybeTuple
       , Algebra, Coalgebra, topDown, bottomUp
       , ifState, bind2
       ) where

import Relude
import Control.Monad.RWS.Lazy (RWST)
import Data.List ((!!))
import Data.Functor.Foldable

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

mapp :: (a -> b) -> [[a]] -> [[b]]
mapp = map . map

zippWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zippWith = zipWith . zipWith

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f a b c = getZipList $ f <$> ZipList a <*> ZipList b <*> ZipList c

toStream :: [Maybe a] -> NonEmpty (Maybe a)
toStream = fromJust . nonEmpty . (<> repeat Nothing)

maybeAny :: (n -> n -> n) -> Maybe n -> Maybe n -> Maybe n
maybeAny f (Just a) (Just b) = Just $ f a b
maybeAny _ a Nothing = a
maybeAny _ Nothing b = b

maybeMin :: Ord n => Maybe n -> Maybe n -> Maybe n
maybeMin = maybeAny min

maybePlus :: Num n => Maybe n -> Maybe n -> Maybe n
maybePlus = maybeAny (+)

liftMaybeTuple :: (Maybe a, b) -> Maybe (a, b)
liftMaybeTuple (Just a, b) = Just (a, b)
liftMaybeTuple _ = Nothing

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

topDown, bottomUp :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
topDown f  = Fix <<< fmap (topDown f) <<< unfix <<< f
bottomUp f = unfix >>> fmap (bottomUp f) >>> Fix >>> f

ifState :: (Monad m, Monoid w) => (s -> Bool)
        -> RWST r w s m a -> RWST r w s m a -> RWST r w s m a
ifState p t f = do b <- gets p
                   if b then t else f

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f x y = liftA2 (,) x y >>= uncurry f

chunks :: Int -> [a] -> [[a]]
chunks dims = unfoldr f
  where f :: [a] -> Maybe ([a], [a])
        f [] = Nothing
        f ps = Just $ splitAt dims ps

fst3  :: (a, b, c) -> a
fst3  (a, _, _) = a
snd3  :: (a, b, c) -> b
snd3  (_, b, _) = b
thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

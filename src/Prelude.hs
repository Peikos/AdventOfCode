-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , atIdx, singleton
       ) where

import Relude
import Data.List ((!!))

singleton :: a -> [a]
singleton = return

atIdx :: [a] -> Int -> Maybe a
atIdx xs i | i < length xs = Just $ xs !! i
           | otherwise   = Nothing


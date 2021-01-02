module Day15 where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Debug.Trace

--readData :: String -> [Integer]
--readData = map read . splitOn ','

sample, input :: [Integer]
sample = [0,3,6]
input = [1,2,16,19,18,0]


ana :: State (Integer, Integer, Map Integer Integer) [Integer]
ana  = do (p, t, m) <- get
          --let n = trace (show (p, t, m)) $ maybe 0 (flip subtract t) $ Map.lookup p m
          let n = maybe 0 (flip subtract t) $ Map.lookup p m
          put (n, succ t, Map.insert p t m)
          (n:) <$> ana

--ana' = put  >> (l:) <$> ana []

{- ana (x:xs) = do (p, t, m) <- get
                put (x, succ t, Map.insert p t m)
                (x:) <$> ana xs
-- eerste lijst zonder recursie, gelijk in map?
-}
            --let (mc, m') = Map.insertLookupWithKey (\_ x _ -> x) p 

start l = (last l, fromIntegral $ pred $ length l, Map.fromList $ flip zip [0..] $ init $ l)

--foo = evalState (ana sample) 
--bar = evalState (ana input) (0, 0, Map.empty)
chal1 = (!! 2019) . challenge1
chal2 = (!! 29999999) . challenge1
challenge1 lst = lst ++ (evalState ana . start $ lst)

-- foo :: Integer -> Integer
-- foo 0 = 0
-- foo 1 = 3
-- foo 2 = 6
-- foo n = 

main = do --print $ chal2 sample
          print $ chal2 input

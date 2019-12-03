module Main (main) where

import qualified First
import qualified Second
import qualified Third

main :: IO ()
main = do putStrLn "# Day 1"
          _ <- First.firstAnswer
          _ <- First.secondAnswer
          putStrLn "# Day 2"
          _ <- Second.firstAnswer
          _ <- Second.secondAnswer
          putStrLn "# Day 3"
          Third.firstAnswer >>= print
          Third.secondAnswer >>= print
          return ()

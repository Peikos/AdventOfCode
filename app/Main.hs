module Main (main) where

import qualified First
import qualified Second
import qualified Third
import qualified Fourth
import qualified Fifth

main :: IO ()
main = do putStrLn "# Day 1"
          _ <- First.firstAnswer
          _ <- First.secondAnswer
          putStrLn "# Day 2"
          _ <- Second.firstAnswer
          _ <- Second.secondAnswer
          putStrLn "# Day 3"
          _ <- Third.firstAnswer
          _ <- Third.secondAnswer
          putStrLn "# Day 4"
          let _ = Fourth.firstAnswer
          let _ = Fourth.secondAnswer
          putStrLn "# Day 5"
          _ <- Fifth.firstAnswer
          _ <- Fifth.secondAnswer
          return ()

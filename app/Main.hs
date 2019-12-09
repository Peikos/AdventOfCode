module Main (main) where

import qualified First
import qualified Second
import qualified Third
import qualified Fourth
import qualified Fifth
import qualified Sixth
import qualified Seventh
import qualified Eighth
import qualified Ninth

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
          putStrLn "# Day 6"
          _ <- Sixth.firstAnswer
          _ <- Sixth.secondAnswer
          putStrLn "# Day 7"
          _ <- Seventh.firstAnswer
          _ <- Seventh.secondAnswer
          putStrLn "# Day 8"
          _ <- Eighth.firstAnswer
          _ <- Eighth.secondAnswer
          putStrLn "# Day 9"
          _ <- Ninth.firstAnswer
          Ninth.secondAnswer >>= print
          return ()

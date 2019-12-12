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
import qualified Tenth
import qualified Eleventh
import Twelfth

main :: IO ()
main = do putStrLn "# Day 1"
          _ <- First.d1p1
          _ <- First.d1p2
          putStrLn "# Day 2"
          _ <- Second.d2p1
          _ <- Second.d2p2
          putStrLn "# Day 3"
          _ <- Third.d3p1
          _ <- Third.d3p2
          putStrLn "# Day 4"
          let _ = Fourth.d4p1
          let _ = Fourth.d4p2
          putStrLn "# Day 5"
          _ <- Fifth.d5p1
          _ <- Fifth.d5p2
          putStrLn "# Day 6"
          _ <- Sixth.d6p1
          _ <- Sixth.d6p2
          putStrLn "# Day 7"
          _ <- Seventh.d7p1
          _ <- Seventh.d7p1
          putStrLn "# Day 8"
          _ <- Eighth.d8p1
          _ <- Eighth.d8p2 -- printImage
          putStrLn "# Day 9"
          _ <- Ninth.d9p1
          _ <- Ninth.d9p2
          putStrLn "# Day 10"
          _ <- Tenth.d10p1
          _ <- Tenth.d10p2
          putStrLn "# Day 11"
          _ <- Eleventh.d11p1
          _ <- Eleventh.d11p2 -- printPainting
          putStrLn "# Day 12"
          print d12p1
          print d12p2
          return ()

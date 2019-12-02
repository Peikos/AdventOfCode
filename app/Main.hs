module Main (main) where

import qualified First
import qualified Second

main :: IO ()
main = do First.firstAnswer >>= print
          First.secondAnswer >>= print
          Second.firstAnswer >>= print
          Second.secondAnswer >>= print

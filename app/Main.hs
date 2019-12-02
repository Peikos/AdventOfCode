module Main (main) where

import First

main :: IO ()
main = do firstAnswer >>= print
          secondAnswer >>= print

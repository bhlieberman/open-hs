module Main where

import qualified Substances (getResults)

main :: IO ()
main = do
  Substances.getResults

module Main (main) where

import Test.HUnit
import Util.ParsingTest (parsingTests)

main :: IO ()
main = do
  counts <- runTestTT parsingTests
  print counts

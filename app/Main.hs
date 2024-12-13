module Main where

import AoC.Day05 (solution)

main :: IO ()
main = do
  contents <- readFile "input/day05_input.txt"
  let toLines = lines contents

  print (solution toLines)

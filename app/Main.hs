module Main where

import AoC.Day05 (parseInput, toGraphs)
import qualified Util.Graph

main :: IO ()
main = do
  contents <- readFile "input/day05_example.txt"
  let toLines = lines contents

  let (orderings, updates) = parseInput toLines
  print orderings
  print (head updates)

  let graphs = toGraphs orderings updates
  print graphs

  let sorted = map Util.Graph.topoSort graphs
  print sorted

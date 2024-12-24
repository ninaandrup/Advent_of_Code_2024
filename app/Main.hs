module Main where

import AoC.Day05 (solution1, solution2)
import AoC.Day06 (solution1, solution2)
import AoC.Day07 (solution1, solution2)
import AoC.Day10 (solution1, solution2)
import AoC.Day11 (solution1, solution2)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, tpe] ->
      ( do
          let tpe' = if tpe == "i" then "_input" else "_example"
          contents <- readFile ("input/day" ++ day ++ tpe' ++ ".txt")
          let contentLines = lines contents

          let toPrint sol1 sol2 = putStrLn ("Part I: " ++ show (sol1 contentLines) ++ "\nPart II: " ++ show (sol2 contentLines))

          case day of
            "05" -> toPrint AoC.Day05.solution1 AoC.Day05.solution2
            "06" -> toPrint AoC.Day06.solution1 AoC.Day06.solution2
            "07" -> toPrint AoC.Day07.solution1 AoC.Day07.solution2
            "10" -> toPrint AoC.Day10.solution1 AoC.Day10.solution2
            "11" -> toPrint AoC.Day11.solution1 AoC.Day11.solution2
            _ -> putStrLn "Day is not implemented"
      )
    _ -> putStrLn "Wrong number of arguments"

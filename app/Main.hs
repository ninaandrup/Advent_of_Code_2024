module Main where

import AoC.Day05 (solution)
import AoC.Day06 (solution)
import AoC.Day07 (solution1, solution2)
import AoC.Day09 (solution1, solution2)
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

          let toPrint sol1 sol2 = "Part I: " ++ show sol1 ++ "\nPart II: " ++ show sol2

          case day of
            "05" -> print (AoC.Day05.solution contentLines)
            "06" -> print (AoC.Day06.solution contentLines)
            "07" -> putStrLn (toPrint (AoC.Day07.solution1 contentLines) (AoC.Day07.solution2 contentLines))
            "09" -> putStrLn (toPrint (AoC.Day09.solution1 contentLines) (AoC.Day09.solution2 contentLines))
            _ -> putStrLn "Day is not implemented"
      )
    _ -> putStrLn "Wrong number of arguments"

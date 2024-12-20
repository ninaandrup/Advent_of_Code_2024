module Main where

import AoC.Day05 (solution)
import AoC.Day06 (solution)
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

          case day of
            "05" -> print (AoC.Day05.solution contentLines)
            "06" -> print (AoC.Day06.solution contentLines)
            _ -> putStrLn "Day is not implemented"
      )
    _ -> putStrLn "Wrong number of arguments"

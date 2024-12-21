module AoC.Day07 (solution) where

import Data.List.Split (splitOn)
import Util.Parsing (splitStringAt)

data Operations = Add | Mult

type Equation = (Int, [Int])

parseEquation :: String -> Equation
parseEquation str =
  let (res, values) = splitStringAt ':' str
      values' = splitOn " " values
   in (read res, map read (tail values'))

solution :: [String] -> [Equation]
solution = map parseEquation

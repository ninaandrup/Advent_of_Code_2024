module AoC.Day07 (solution) where

import Data.List.Split (splitOn)
import Util.Parsing (splitStringAt)

data Operation = Add | Mult deriving (Show)

type Equation = (Int, [Int])

compute :: Int -> Int -> Operation -> Int
compute x y Add = x + y
compute x y Mult = x * y

operationCombinations :: Int -> [[Operation]]
operationCombinations 0 = [[]]
operationCombinations n =
  [x : xs | x <- [Add, Mult], xs <- operationCombinations (n - 1)]

equationHolds :: Equation -> [Operation] -> Bool
equationHolds (res, [x, y]) [oper] = res == compute x y oper
equationHolds (res, x : y : valueTail) (oper : operTail) =
  equationHolds (res, compute x y oper : valueTail) operTail
equationHolds _ _ = False

equationPossible :: Equation -> Bool
equationPossible equation =
  any (equationHolds equation) (operationCombinations (length (snd equation) - 1))

parseEquation :: String -> Equation
parseEquation str =
  let (testValue, values) = splitStringAt ':' str
      values' = splitOn " " values
   in (read testValue, map read (tail values'))

solution :: [String] -> [Equation]
solution = map parseEquation

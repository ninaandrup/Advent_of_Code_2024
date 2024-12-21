module AoC.Day07 (solution1, solution2) where

import Data.List.Split (splitOn)
import Util.Parsing (splitStringAt)

type Equation = (Int, [Int])

parseEquation :: String -> Equation
parseEquation str =
  let (testValue, values) = splitStringAt ':' str
      values' = splitOn " " values
   in (read testValue, map read (tail values'))

data Operation = Add | Mult | Concat deriving (Show)

compute :: Int -> Int -> Operation -> Int
compute x y Add = x + y
compute x y Mult = x * y
compute x y Concat = read (show x ++ show y)

operationCombinations :: Int -> [[Operation]]
operationCombinations 0 = [[]]
operationCombinations n =
  [x : xs | x <- [Add, Mult], xs <- operationCombinations (n - 1)]

operationExtendedCombinations :: Int -> [[Operation]]
operationExtendedCombinations 0 = [[]]
operationExtendedCombinations n =
  [x : xs | x <- [Add, Mult, Concat], xs <- operationExtendedCombinations (n - 1)]

equationHolds :: Equation -> [Operation] -> Bool
equationHolds (res, [x, y]) [oper] = res == compute x y oper
equationHolds (res, x : y : valueTail) (oper : operTail) =
  equationHolds (res, compute x y oper : valueTail) operTail
equationHolds _ _ = False

equationPossible :: (Int -> [[Operation]]) -> Equation -> Bool
equationPossible combinationGen equation =
  any (equationHolds equation) (combinationGen (length (snd equation) - 1))

sumEquations :: (Int -> [[Operation]]) -> [Equation] -> Int
sumEquations combinationGen equations =
  sum (map fst (filter (equationPossible combinationGen) equations))

solution1 :: [String] -> Int
solution1 = sumEquations operationCombinations . map parseEquation

solution2 :: [String] -> Int
solution2 input =
  solution1 input
    + sumEquations operationExtendedCombinations (filter (not . equationPossible operationCombinations) (map parseEquation input))

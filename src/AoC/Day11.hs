module AoC.Day11 (solution1, solution2) where

import Data.List.Split (splitOn)

solution1 :: [String] -> Int
solution1 = length . blinks 25 . parser

parser :: [String] -> [Int]
parser = map read . splitOn " " . head

blinks :: Int -> [Int] -> [Int]
blinks 0 ls = ls
blinks x ls = blinks (x - 1) (blink ls)

blink :: [Int] -> [Int]
blink [] = []
blink (x : xs)
  | x == 0 = 1 : blink xs
  | evenDigits x = let (a, b) = splitInTwo x in a : b : blink xs
  | otherwise = x * 2024 : blink xs

evenDigits :: Int -> Bool
evenDigits = even . length . show

splitInTwo :: Int -> (Int, Int)
splitInTwo x =
  let x' = show x
      len = length x' `div` 2
   in (read (take len x'), read (drop len x'))

solution2 :: [String] -> Int
solution2 = length . blinks 75 . parser

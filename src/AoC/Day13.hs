module AoC.Day13 where

import Data.List.Split (splitOneOf)

data Coeffs = Coeffs {ac :: Int, bc :: Int, res :: Int} deriving (Show)

data Res = Res {a :: Int, b :: Int} deriving (Show)

solution1 :: [String] -> Int
solution1 input =
  sum $ map (cost . uncurry solveEquations) (parser ("" : input))

solution2 :: [String] -> Int
solution2 input =
  sum $ map (cost . uncurry solveEquations . add10000000000000T) (parser ("" : input))

parser :: [String] -> [(Coeffs, Coeffs)]
parser (_ : buttonA : buttonB : prize : rest) =
  (Coeffs {ac = a1, bc = b1, res = r1}, Coeffs {ac = a2, bc = b2, res = r2}) : parser rest
  where
    (a1, a2) = parseLine "+," buttonA
    (b1, b2) = parseLine "+," buttonB
    (r1, r2) = parseLine "=," prize
parser _ = []

parseLine :: String -> String -> (Int, Int)
parseLine sep str =
  (a1, a2)
  where
    ls = splitOneOf sep str
    a1 = read (ls !! 1) :: Int
    a2 = read (ls !! 3) :: Int

add10000000000000 :: Coeffs -> Coeffs
add10000000000000 c =
  Coeffs {ac = ac c, bc = bc c, res = res c + 10000000000000}

add10000000000000T :: (Coeffs, Coeffs) -> (Coeffs, Coeffs)
add10000000000000T (c1, c2) = (add10000000000000 c1, add10000000000000 c2)

solveEquations :: Coeffs -> Coeffs -> Maybe Res
solveEquations (Coeffs {ac = ac1, bc = bc1, res = r1}) (Coeffs {ac = ac2, bc = bc2, res = r2})
  | determinant == 0 = Nothing -- No unique solution
  | aIsInt && bIsInt = Just Res {a = a', b = b'}
  | otherwise = Nothing -- No integer solution
  where
    determinant = ac1 * bc2 - ac2 * bc1
    aNum = r1 * bc2 - r2 * bc1
    bNum = ac1 * r2 - ac2 * r1
    aIsInt = aNum `mod` determinant == 0
    bIsInt = bNum `mod` determinant == 0
    a' = aNum `div` determinant
    b' = bNum `div` determinant

cost :: Maybe Res -> Int
cost r =
  case r of
    Nothing -> 0
    Just Res {a = a', b = b'} -> a' * 3 + b'

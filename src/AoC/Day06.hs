module AoC.Day06 (debugger, solution) where

import Control.Applicative ((<|>))
import Util.Lib ((!?))

data Direction = North | East | South | West deriving (Show)

type Position = (Int, Int)

type Row = [Bool]

type Grid = [Row]

type GuardPosition = (Position, Direction)

parseLine :: Row -> Maybe GuardPosition -> Int -> Int -> String -> (Row, Maybe GuardPosition)
parseLine acc guardPos _ _ [] = (acc, guardPos)
parseLine acc guardPos xPos yPos (x : xs) =
  case x of
    '^' -> parseLine (acc ++ [False]) (Just ((xPos, yPos), North)) (xPos + 1) yPos xs
    '.' -> parseLine (acc ++ [False]) guardPos (xPos + 1) yPos xs
    '#' -> parseLine (acc ++ [True]) guardPos (xPos + 1) yPos xs
    _ -> ([], Nothing)

parser :: [String] -> (Grid, Maybe GuardPosition)
parser input =
  snd
    ( foldr
        ( \str (idx, (grid, guardPos)) ->
            let (row, guardPos') = parseLine [] Nothing 0 idx str
             in (idx - 1, (row : grid, guardPos <|> guardPos'))
        )
        (length input - 1, ([], Nothing))
        input
    )

hasObstacle :: Grid -> Position -> Maybe Bool
hasObstacle grid (x, y) =
  (grid !? y) >>= (!? x)

turn90Dec :: Direction -> Direction
turn90Dec direction =
  case direction of
    North -> East
    East -> South
    South -> West
    West -> North

turnGuard90Dec :: GuardPosition -> GuardPosition
turnGuard90Dec ((x, y), direction) =
  ((x, y), turn90Dec direction)

nextPosition :: GuardPosition -> GuardPosition
nextPosition ((x, y), direction) =
  case direction of
    North -> ((x, y - 1), direction)
    East -> ((x + 1, y), direction)
    South -> ((x, y + 1), direction)
    West -> ((x - 1, y), direction)

moveGuard :: Grid -> GuardPosition -> Maybe GuardPosition
moveGuard grid pos =
  let (pos', direction) = nextPosition pos
   in case hasObstacle grid pos' of
        Nothing -> Nothing
        Just b ->
          ( if b
              then moveGuard grid (turnGuard90Dec pos)
              else Just (pos', direction)
          )

allPositions :: [Maybe GuardPosition] -> Grid -> GuardPosition -> [Maybe GuardPosition]
allPositions acc grid guardPos =
  let newPos = moveGuard grid guardPos
      acc' = acc ++ [newPos]
   in case newPos of
        Nothing -> acc'
        Just guardPos' -> allPositions acc' grid guardPos'

debugger :: [String] -> [Maybe GuardPosition]
debugger input =
  let (grid, startPos) = parser input
   in case startPos of
        Nothing -> []
        Just guardPos -> allPositions [startPos] grid guardPos

countPositions :: Int -> Grid -> GuardPosition -> Int
countPositions count grid guardPos =
  let newPos = moveGuard grid guardPos
      count' = count + 1
   in case newPos of
        Nothing -> count'
        Just guardPos' -> countPositions count' grid guardPos'

solution :: [String] -> Int
solution input =
  let (grid, startPos) = parser input
   in case startPos of
        Nothing -> 0
        Just guardPos -> countPositions 0 grid guardPos

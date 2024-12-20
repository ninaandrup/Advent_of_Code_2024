module AoC.Day06 (solution) where

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

isVisited :: Grid -> Position -> Bool
isVisited grid (x, y) =
  (grid !! y) !! x

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

visit :: (Int, Int) -> Grid -> Grid
visit (x, y) grid =
  take y grid ++ [visitRow x (grid !! y)] ++ drop (y + 1) grid
  where
    visitRow i row = take i row ++ [True] ++ drop (i + 1) row

countPositions :: Int -> Grid -> Grid -> GuardPosition -> Int
countPositions count visitedGrid grid guardPos =
  let newPos = moveGuard grid guardPos
   in case newPos of
        Nothing -> count
        Just guardPos' ->
          if isVisited visitedGrid (fst guardPos')
            then
              countPositions count visitedGrid grid guardPos'
            else
              let visitedGrid' = visit (fst guardPos') visitedGrid
               in countPositions (count + 1) visitedGrid' grid guardPos'

solution :: [String] -> Int
solution input =
  let (grid, startPos) = parser input
      visitedGrid = map (map (const False)) grid
   in case startPos of
        Nothing -> 0
        Just guardPos -> countPositions 1 (visit (fst guardPos) visitedGrid) grid guardPos

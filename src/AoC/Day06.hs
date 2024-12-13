module AoC.Day06 (solution) where

data Direction = North | East | South | West

type Position = (Int, Int)

type Grid = [[Bool]]

type GuardPosition = (Position, Direction)

hasObstacle :: Grid -> Position -> Bool
hasObstacle grid (x, y) =
  (grid !! x) !! y

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
    North -> ((x - 1, y), direction)
    East -> ((x, y + 1), direction)
    South -> ((x + 1, y), direction)
    West -> ((x, y - 1), direction)

moveGuard :: Grid -> GuardPosition -> GuardPosition
moveGuard grid pos =
  let (pos', direction) = nextPosition pos
   in if hasObstacle grid pos'
        then nextPosition (turnGuard90Dec pos)
        else (pos', direction)

solution :: [String] -> Int
solution input = 0

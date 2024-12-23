module AoC.Day10 where

import Util.Graph (Edge, Graph, Vertex, addEdge, bfs)

data Position = Position {idx :: Int, height :: Int} deriving (Show, Eq, Ord)

parseLine :: Int -> String -> [Position]
parseLine start = zipWith (\j x -> Position {idx = j, height = read [x] :: Int}) [start ..]

parseInput :: [String] -> [[Position]]
parseInput input =
  zipWith
    (\i ls -> parseLine (i * length (head input)) ls)
    [0 ..]
    input

neighbours :: [Position] -> [Position] -> [(Position, Position)]
neighbours [x] (y : _) = [(x, y)]
neighbours (x : x' : xs) [] = (x, x') : neighbours (x' : xs) []
neighbours (x : x' : xs) (y : ys) =
  (x, x') : (x, y) : neighbours (x' : xs) ys
neighbours _ _ = []

pairPositions :: [[Position]] -> [(Position, Position)]
pairPositions positions =
  concat (zipWith neighbours positions (tail positions ++ [[]]))

toEdges :: (Position, Position) -> Maybe (Edge Position)
toEdges (pos1, pos2)
  | height pos1 == height pos2 + 1 = Just (pos2, pos1)
  | height pos1 + 1 == height pos2 = Just (pos1, pos2)
  | otherwise = Nothing

toGraph :: [[Position]] -> Graph Position
toGraph positions =
  let pairs = pairPositions positions
      edges = map toEdges pairs
   in foldr
        ( \edge acc -> case edge of
            Just e -> addEdge e acc
            Nothing -> acc
        )
        []
        edges

reachableTops :: Graph Position -> [[Vertex Position]]
reachableTops graph =
  let startPositions = filter (\v -> height v == 0) (map fst graph)
      reachableFromBottom = map (`bfs` graph) startPositions
   in map (filter (\v -> height v == 9)) reachableFromBottom

solution1 :: [String] -> Int
solution1 input = sum (map length (reachableTops (toGraph (parseInput input))))

solution2 :: [String] -> Int
solution2 input = 0

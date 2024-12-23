module Util.Graph where

import qualified Data.Set as Set

type Vertex a = a

type Edge a = (Vertex a, Vertex a)

type Graph a = [(Vertex a, [Vertex a])]

addEdge :: (Eq a) => Edge a -> Graph a -> Graph a
addEdge (src, dest) [] = [(src, [dest])]
addEdge (src, dest) ((x, xs) : ys) =
  if src == x
    then
      (x, dest : xs) : ys
    else
      (x, xs) : addEdge (src, dest) ys

empty :: (Eq a) => Graph a -> Bool
empty = (== [])

notEmpty :: (Eq a) => Graph a -> Bool
notEmpty = (/= [])

getSources :: (Ord a) => Graph a -> [Vertex a]
getSources g =
  let toNodes = foldr (Set.union . foldr Set.insert Set.empty . snd) Set.empty g
   in filter (`Set.notMember` toNodes) (map fst g)

topoHelper :: (Ord a) => [Vertex a] -> Graph a -> Maybe [Vertex a]
topoHelper sorted g =
  let sources = getSources g
   in case sources of
        [] -> if notEmpty g then Nothing else Just (reverse sorted)
        x : _ -> topoHelper (x : sorted) (filter ((/= x) . fst) g)

topoSort :: (Ord a) => Graph a -> Maybe [Vertex a]
topoSort =
  topoHelper []

getNeighbours :: (Eq a) => Vertex a -> Graph a -> [Vertex a]
getNeighbours _ [] = []
getNeighbours v ((v', vs) : graph) =
  if v == v'
    then vs
    else getNeighbours v graph

bfs :: (Ord a) => Vertex a -> Graph a -> [Vertex a]
bfs root =
  bfsHelper [root] (Set.singleton root)

bfsHelper :: (Ord a) => [Vertex a] -> Set.Set (Vertex a) -> Graph a -> [Vertex a]
bfsHelper [] visited _ = Set.toList visited
bfsHelper (v : queue) visited graph =
  let neighbours = getNeighbours v graph
      notVisitedNeighBours = filter (`Set.notMember` visited) neighbours
      queue' = queue ++ notVisitedNeighBours
      visited' = foldr Set.insert visited notVisitedNeighBours
   in bfsHelper queue' visited' graph

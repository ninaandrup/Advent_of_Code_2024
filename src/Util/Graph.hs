module Util.Graph (Edge, Vertex, Graph, empty, notEmpty, topoSort) where

import qualified Data.Set as Set

type Vertex = String

type Edge = (Vertex, Vertex)

type Graph = [(Vertex, [Vertex])]

empty :: Graph -> Bool
empty = (== [])

notEmpty :: Graph -> Bool
notEmpty = (/= [])

getSources :: Graph -> [Vertex]
getSources g =
  let toNodes = foldr (Set.union . foldr Set.insert Set.empty . snd) Set.empty g
   in filter (`Set.notMember` toNodes) (map fst g)

topoHelper :: [Vertex] -> Graph -> Maybe [Vertex]
topoHelper sorted g =
  let sources = getSources g
   in case sources of
        [] -> if notEmpty g then Nothing else Just (reverse sorted)
        x : _ -> topoHelper (x : sorted) (filter ((/= x) . fst) g)

topoSort :: Graph -> Maybe [Vertex]
topoSort =
  topoHelper []

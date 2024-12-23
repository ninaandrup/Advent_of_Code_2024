module AoC.Day05 (solution1, solution2) where

import qualified Data.Set as Set
import Util.Graph (Graph, topoSort)
import Util.Parsing (splitStringAt)

type Page = String

type Order = (Page, Page)

type Orderings = [Order]

type Update = [Page]

type Updates = [Update]

parseOrdering :: String -> Order
parseOrdering = splitStringAt '|'

parseUpdate :: String -> Update
parseUpdate update =
  let helper acc str =
        let (before, rest) = splitStringAt ',' str
            new_acc = acc ++ [before]
         in if rest == ""
              then new_acc
              else helper new_acc rest
   in helper [] update

parseInputHelper :: [Order] -> [String] -> (Orderings, Updates)
parseInputHelper _ [] = ([], [])
parseInputHelper acc (next : rest) =
  if next == ""
    then (acc, map parseUpdate rest)
    else parseInputHelper (acc ++ [parseOrdering next]) rest

parseInput :: [String] -> (Orderings, Updates)
parseInput = parseInputHelper []

getDestinations :: Orderings -> Update -> Page -> [Page]
getDestinations orderings update page =
  let vertices = foldr Set.insert Set.empty update
      helper acc rest =
        case rest of
          [] -> acc
          (src, dst) : tl ->
            if src == page && dst `Set.member` vertices
              then helper (acc ++ [dst]) tl
              else helper acc tl
   in helper [] orderings

toGraphs :: Orderings -> Updates -> [Graph]
toGraphs orderings =
  map (\update -> zip update (map (getDestinations orderings update) update))

summation :: Updates -> [Graph] -> Int
summation updates graphs =
  let helper acc rest =
        case rest of
          [] -> acc
          (_, Nothing) : tl -> helper acc tl
          (update, Just graph) : tl ->
            if update == graph
              then helper acc tl
              else
                let acc' = acc + (read (graph !! (length graph `div` 2)) :: Int)
                 in helper acc' tl
   in helper 0 (zip updates (map topoSort graphs))

solution1 :: [String] -> Int
solution1 input = 0

solution2 :: [String] -> Int
solution2 input =
  let (orderings, updates) = parseInput input
      graphs = toGraphs orderings updates
   in summation updates graphs

module Util.Parsing where

splitStringAt :: Char -> String -> (String, String)
splitStringAt c str =
  let (before, rest) = break (== c) str
   in (before, drop 1 rest)

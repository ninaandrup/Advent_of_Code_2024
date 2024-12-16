module Util.Lib where

(!?) :: [a] -> Int -> Maybe a
(!?) ls idx =
  if idx < 0 || length ls <= idx
    then
      Nothing
    else
      Just (ls !! idx)

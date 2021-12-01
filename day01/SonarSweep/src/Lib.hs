module Lib
  ( numDepthIncreases,
  )
where

numDepthIncreases :: Ord a => [a] -> Int
numDepthIncreases [] = 0
numDepthIncreases [_] = 0
numDepthIncreases (x : y : xs) =
  let next = numDepthIncreases (y : xs)
   in if y > x then 1 + next else next

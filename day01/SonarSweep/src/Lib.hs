module Lib
  ( numDepthIncreases,
  )
where

numDepthIncreases :: [Int] -> Int
numDepthIncreases [] = 0
numDepthIncreases [_] = 0
numDepthIncreases (x : y : xs) =
  let next = numDepthIncreases (y : xs)
   in if y > x then 1 + next else next

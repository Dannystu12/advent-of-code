module Lib
  ( getPos,
    Command (..),
  )
where

data Command = Forward Int | Down Int | Up Int

getPos :: [Command] -> (Int, Int)
getPos = 
    foldl accum (0, 0)
    where
        accum (hpos, depth) (Forward x) = (hpos + x, depth) 
        accum (hpos, depth) (Up x) = (hpos, depth - x)
        accum (hpos, depth) (Down x) = (hpos, depth + x)

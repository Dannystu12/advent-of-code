module Lib
  ( getPos,
    Command (..),
  )
where

data Command = Forward Int | Down Int | Up Int

getPos :: [Command] -> (Int, Int)
getPos = undefined

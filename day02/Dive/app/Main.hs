module Main where

import Control.Monad
import Lib (Command, getPos)
import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs.txt" ReadMode
  contents <- hGetContents handle

  let commands = readCommands $ lines contents
  putStrLn $ "Commands Loaded: " ++ (show $ length $ commands)

  let pos@(h, d) = getPos commands
  putStrLn $ "Pos: " ++ (show pos)
  putStrLn $ "Horizontal * Depth: " ++ show (h * d)
  where
    readCommands :: [String] -> [Command]
    readCommands = map read

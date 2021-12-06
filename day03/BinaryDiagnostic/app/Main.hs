module Main where

import Control.Monad
import Data.Char (digitToInt, isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe
import Lib

import System.IO

main :: IO ()
main = do
  handle <- openFile "inputs.txt" ReadMode
  contents <- hGetContents handle

  let diagnostics = readDiagnostics $ lines contents

  let pc = getPowerConsumption diagnostics
  putStrLn $ "The power consumption is: " ++ show pc
  where
    readDiagnostics :: [String] -> [BinaryNum]
    readDiagnostics nums = if all isJust res then map fromJust res else error "Invalid"
      where
        res = map (stringToBn . trim) nums

    trim = dropWhileEnd isSpace . dropWhile isSpace
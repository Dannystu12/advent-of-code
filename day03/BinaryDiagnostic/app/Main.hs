module Main where

import Control.Monad
import Data.Char (digitToInt, isSpace)
import Data.List (dropWhileEnd)
import Lib
import System.IO
import GHC.Exception (throw)

main :: IO ()
main = do
  handle <- openFile "inputs.txt" ReadMode
  contents <- hGetContents handle

  let diagnostics = readDiagnostics $ lines contents
  putStrLn $ show diagnostics
  
  where
    readDiagnostics :: [String] -> [String]
    readDiagnostics s =
      if all (== True) $ map (all (`elem` "10")) num
        then num
        else error "Could not parse"
      where
        num = map trim s

    trim = dropWhileEnd isSpace . dropWhile isSpace
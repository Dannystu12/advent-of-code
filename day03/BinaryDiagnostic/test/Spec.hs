module Main where

import Data.Maybe (fromJust)
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "BinaryDiagnostic Tests"
      [getPowerConsumptionTests]

getPowerConsumptionTests =
  testGroup
    "getPowerConsumptionTests"
    [ testCase "Empty list returns 0" $
        getPowerConsumption [] @?= 0,
      testCase "Test 0" $
        getPowerConsumption [getBn "0000"] @?= 0,
      testCase "Test 1" $
        getPowerConsumption [getBn "1111"] @?= 0,
      testCase "Test Basic" $
        getPowerConsumption [getBn "0010"] @?= 2 * 13,
      testCase
        "Test full"
        $ getPowerConsumption
          [ getBn "11110",
            getBn
              "10110",
            getBn
              "10111",
            getBn
              "10101",
            getBn
              "01111",
            getBn
              "00111",
            getBn
              "11100",
            getBn
              "10000",
            getBn
              "11001",
            getBn
              "00010",
            getBn
              "01010"
          ]
          @?= 198
    ]
  where
    getBn = fromJust . stringToBn

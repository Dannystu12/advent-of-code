module Main where

import Data.Maybe (fromJust)
import Lib
import Lib (bnToInt)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "BinaryDiagnostic Tests"
      [bnToIntTests, getEpsilonTests, getGammaTests, getPowerConsumptionTests]

bnToIntTests =
  testGroup
    "bnToIntTests"
    [ testCase "Test 0" $ (bnToInt $ getBn "0") @?= 0,
      testCase "Test 1" $ (bnToInt $ getBn "1") @?= 1,
      testCase "Test 0101" $ bnToInt (getBn "0101") @?= 5
    ]

getGammaTests =
  testGroup
    "getGammaTests"
    [ testCase "Test all empty" $
        getGamma [] @?= getBn "",
      testCase "Test 0" $
        getGamma [getBn "0"] @?= getBn "0",
      testCase "Test 1" $
        getGamma [getBn "1"] @?= getBn "1",
      testCase "Test multiple numbers (even)" $
        getGamma
          [ getBn "1",
            getBn
              "0"
          ]
          @?= getBn "0",
      testCase "Test multiple numbers (odd)" $
        getGamma
          [ getBn "1",
            getBn
              "0",
            getBn "1"
          ]
          @?= getBn "1",
      testCase "Test bigger set of numbers" $
        getGamma
          [ getBn
              "00100",
            getBn
              "11110",
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
          @?= getBn "10110"
    ]

getEpsilonTests =
  testGroup
    "getEpsilonTests"
    [ testCase "Test all empty" $
        getEpsilon (getBn "") @?= getBn "",
      testCase "Test all 0" $
        getEpsilon (getBn "0000") @?= getBn "1111",
      testCase "Test all 1" $
        getEpsilon (getBn "1111") @?= getBn "0000",
      testCase "Test num 4 is 11" $
        getEpsilon (getBn "0100") @?= getBn "1011"
    ]

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
          [ getBn "00100",
            getBn "11110",
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

getBn = fromJust . stringToBn

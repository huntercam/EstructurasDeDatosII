module ArrTests where

import Test.HUnit
import Seq
import Arr        (Arr)
import ArrSeq


s0, s1, s2, s3, s4, s5 :: Arr Int
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]
s4 = fromList [0,0,0,0]
s5 = fromList [-1,10,-10,1]

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)
testMap2 ::Test   
testMap2 = 
  TestCase $ assertEqual "Error on testMap2"
                         (fromList [0,10,-10,2]) (mapS (\x-> if even x then x else x+1) s5)                

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduce2 :: Test
testReduce2 = 
  TestCase $ assertEqual "Error testReduce2"
                         1 (reduceS (-) 1 s4)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testScan2 :: Test
testScan2 = 
  TestCase $ assertEqual "Error on testScan2"
                         (fromList[0,-1,9,-1], 0) (scanS (+) 0 s5)

testsArray = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq3,
    testMap2,
    testReduce2,
    testScan2
  ]


main :: IO Counts
main = runTestTT $ TestList testsArray

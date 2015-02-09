module Lect7 where

import Test.HUnit
import Test.QuickCheck

addOne :: [Int] -> [Int]
addOne n = map ((+)1) n

at1 = TestCase (assertEqual "addOne [1,2,3]" (addOne [1,2,3]) [2,3,4])
at2 = TestCase (assertEqual "addOne []" (addOne []) [])
addOneTests = TestList[at1, at2]


addN:: Int -> [Int] -> [Int]
addN n is = map ((+)n) is

ant1 = TestCase (assertEqual "addN 3 [3,4,5]" (addN 3 [3,4,5]) [6,7,8])
ant2 = TestCase (assertEqual "addN 0 [3,4,5]" (addN 0 [3,4,5]) [3,4,5])
ant3 = TestCase (assertEqual "addN -4 [1,2,3]" (addN (-4) [1,2,3]) [-3,-2,-1::Int])
addNTests = TestList[ant1, ant2, ant3]

prefixSums:: [Int] -> [Int]
prefixSums [] = []
prefixSums x = prefixSums (init x) ++ [sum x]

ps1 = TestCase (assertEqual "prefixSums [1,2,3,4]" (prefixSums [1,2,3,4]) [1,3,6,10])
ps2 = TestCase (assertEqual "prefixSums []" (prefixSums []) [])
prefixTests = runTestTT $ TestList[ps1,ps2]

tak:: Int -> [a] -> [a]
tak 0 xs = []
tak i [] = []
tak i (x:xs) = x : tak (i - 1) xs

t1 = TestCase (assertEqual "tak 3 [1,2,3,4]" (tak 3 [1,2,3,4]) [1,2,3])
takTest = runTestTT $ TestList[t1]

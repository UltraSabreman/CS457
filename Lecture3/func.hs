module CreatingFunctions where

import Data.List
import Data.Array

-- By definition at top level

plus5 x = x + 5

last x = head(reverse x)
inits = (reverse . tail . reverse)

-- By cases

absolute x | x < 0  = -x
           | x >= 0 = x


swap (x,y) | x < y = (x,y)
           | x > y = (y,x)
           | x==y  = (x,y)

swap2 (x,y) | x > y = (y,x)
            | otherwise  = (x,y)

f x y z | x+y == z = True
        | otherwise = False

-- By pattern matching

myand True False = False
myand True True = True
myand False False = False
myand False True = False

myand2 True True = True
myand2 x y = False

-- By us of a Library

smallest = List.minimum [3,7,34,1]


-- by local definition using where or let

ordered = sortBy backwards  [1,76,2,5,9,45]
  where backwards x y = compare y x


-- lambda expression

descending =
   sortBy
   (\ x y -> compare y x)
   [1,76,2,5,9,45]

bySnd =
  groupBy
  (\ (x,y) (m,n) -> y==n)
  [(1,'a'),(3,'a'),(2,'c')]

-- parenthesizing binary operators

six:: Integer
-- 1 + 2 + 3 + 0
six = foldr (+) 0 [1,2,3]

-- By section

add5ToAll = map (+5) [2,3,6,1]


-- by partial application

hasFour = any (==4)
doubleEach = map (\ x -> x+x)

-- By composition

hasTwo = hasFour . doubleEach
empty = (==0) . length

-- By combinator (higher order functions)

k x = \ y -> x

all3s = map (k 3) [1,2,3]

-- By using data and lookup

whatDay x = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"] !! x

first9Primes =  array (1,9)
    (zip [1..9]
         [2,3,5,7,11,13,17,19,23])

nthPrime x = first9Primes ! x

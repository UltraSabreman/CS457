module CreatingListsWithComprehensions  where

import Data.Char(ord)

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.

-- [8,9,10,11,12]
x1 = [8..12]


-- [20,19,18,17,16]
x2 = [ x |  x <- [20, 19 .. 16]]


-- [10,20,30,40,50,60]
x3 = [ x * 10 | x <- [1..6]]

-- [500,450,400,350]
x4 = [ x | x <- [500, 450 .. 350]]


-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]
x5 = [(x, y) | x <- [1,2], y <- [1,2,3]]


-- [(1,99),(2,99),(3,99),(4,99)]
x6 = [(x,y) | x <- [1..4], y <- [99]]


-- -- [(1,2),(2,3),(3,4),(4,5)]
x7 = [ (x, x+1) | x <- [1..4]]


-- [('0',48),('1',49),('2',50),('3',51),('4',52),('5',53)]
-- hint  (ord '0') == 48
x8 = [ (x, (ord x)) | x <- ['0', '1'.. '5'] ]

-- [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
-- nested sequences and comprehensions
x9 = [[1..x] | x <- [1..5]]


days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]


-- ["Monday","Tuesday","Wednesday","Thursday","Friday"]
-- hint use a comprehension with a filter
weekday = [x | x <- days, (head x) /= 'S']


-- [[1,2,3,4,5,6,7]
-- ,[8,9,10,11,12,13,14]
-- ,[15,16,17,18,19,20,21]
-- ,[22,23,24,25,26,27,28]]
-- hint: nest a sequence in a comprehension
--       think about  (i*7 + 1)  and  (i*7 + 7)
x10 = [ [(y*7)+1..(y*7)+7]| y <- [0..3]]


-- ["MonDay","TuesDay","WeDnesDay","ThursDay","FriDay"]
-- Note all lowercase 'd' turned to upper case 'D'
-- hint: nested comprehensions
--        if c=='d' then 'D' else c
x11 = [[if c == 'd' then 'D' else c | c <- x  ]| x <- weekday]

-- write a function, that given an integer, n, returns the sequence
-- of pairs of array indices necessary to perform the bubble
-- sort on an array with indices from 1 to n.
-- bubbleIndices 4 = [(1,2),(2,3),(3,4),(1,2),(2,3),(1,2)]

bubbleIndices:: Int -> [(Int,Int)]
bubbleIndices n = undefined

-- write a function, that given an integer, n, returns the sequence
-- of pairs of array indices necessary to perform the exchange
-- sort on an array with indices from 1 to n.
-- exchangeIndices 5 = [(1,2),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(4,5)]

exchangeIndices:: Int -> [(Int,Int)]
exchangeIndices n = undefined

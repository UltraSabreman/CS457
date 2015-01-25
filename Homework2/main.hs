----------------------------------------------------------------------
CS457/557 Functional Languages                              Homework 2
----------------------------------------------------------------------

Name: Andrey Byelogurov
Email: byelogurov@gmail.com

Due at the start of class on January 19.  You are welcome to work on this
assignment on your own, or with a partner.  In the latter case, turn in
only one copy, but be sure that your submission is clearly labeled with
the names of both partners.

You are welcome (indeed, encouraged!) to use GHCi for help with
all of these questions, but try to predict what the the answers will be
before you try them on the computer ...

Question 1:
(10 points)
-----------
Give possible types for each of the following expressions (or else
explain why you think the expression is ill-typed):
a) map odd Integral a => [a] -> [bool]
b) takeWhile null (a -> bool) -> [a] -> [a]
c) (++[])
d) (:[])
e) ([]:)
f) [ [], [[]], [[[]]], [[[[]]]], [[[[[True]]]]] ]
g) [ [True], [[]], [[[]]], [[[[]]]], [[[[[]]]]] ]
h) map map
i) map (map odd)
j) map . map
k) (map , map)


Question 2:
(10 points)
-----------
Explain what each of the following functions does:
a) odd . (1+)
b) odd . (2*)
c) ([1..]!!)
d) (!!0)
e) reverse . reverse
f) reverse . tail . reverse
g) map reverse . reverse . map reverse


Question 3:
(60 points)
-----------
Complete the following definitions of functions on lists
by deleting or replacing the portions marked with "..."
in each case.  These are intended to be simple, recursive
function definitions; you can find analogs for most of
these functions in the standard Haskell prelude and libraries,
but you should not use those functions here!  Do not worry
about "efficiency", but be sure to run some tests to confirm
that your definitions work as intended.

-- (append xs ys) returns the lists xs and ys appended
-- For example:
--   append [1,2,3] [5,2] = [1,2,3,5,2]

> append            :: [a] -> [a] -> [a]
> append [] ys      = ...
> append (x:xs) ys  = ... (append xs ys) ...

-- addup xs returns the sum of the numbers in
-- the list xs.  For example:
--   addup [1,2,3] = 6

> addup            :: [Int] -> Int
> addup []          = ...
> addup (x:xs)      = ... (addup xs) ...

-- rev xs returns the list of values in xs in
-- reverse order.  For example:
--   rev [1,2,3] = [3,2,1]

> rev       :: [Int] -> [Int]
> rev []     = ...
> rev (x:xs) = ... (rev xs) ...

-- insert n ns inserts the integer n into the list
-- of integers ns.  You can assume that the numbers
-- in the list ns are already arranged in ascending
-- order.  For example:
--   insert 3 [1,2,4,5] = [1,2,3,4,5]
--   insert 5 [1,2,3,4] = [1,2,3,4,5]
--   insert 0 [1,2,3,4] = [0,1,2,3,4]
--   insert 2 [1,2,3,4] = [1,2,2,3,4]
-- As this last example shows, insert does not attempt
-- to eliminate duplicate elements from the list.

> insert         :: Int -> [Int] -> [Int]
> insert x []     = ...
> insert x (y:ys)
>           | ... = ... (insert x ys) ...
>           | ... = ...

-- sort ns returns a sorted version of the list ns.
-- For example:
--   sort [1,2,3,4] = [1,2,3,4]
--   sort [4,3,2,1] = [1,2,3,4]
--   sort [3,1,4,2] = [1,2,3,4]
--   sort [3,2,3,1] = [1,2,3,3]
-- Hint: I suggest that you try to implement an
-- ***insert***ion sort :-)

> sort       :: [Int] -> [Int]
> sort []     = ...
> sort (n:ns) = ... (sort ns) ...

-- splits ns returns the list of all triples (us, v, ws)
-- such that us ++ [v] ++ ws == ns.  For example:
--   splits [0,1,2] = [ ([], 0, [1,2]),
--                      ([0], 1, [2]),
--                      ([0,1], 2, []) ]

> splits       :: [Int] -> [([Int], Int, [Int])]
> splits [x]    = ...
> splits (x:xs) = ... : [ ... | (us, v, ws) <- splits xs ]


Question 4:
(20 points)
-----------
Suppose that n :: Int; explaining how you arrive at your answer,
explain what result you will get from the following expression.

sum [ x - y | x <- [1..n], y <- [1..n] ]

Verify your answer by evaluating this expression for some specific
values of n.


----------------------------------------------------------------------

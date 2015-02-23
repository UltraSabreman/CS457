Sample Mid-term questions
----------------------------------------------------------------------------
Typing expressions

1) For each expression below, write down a valid type that Haskell could 
assign to the that expression. (12 points)


e1:: ___________________________________________

e1 = (True && (3==4),reverse[3],head "abc")


e2:: ___________________________________________

e2 = [] ++ []


e3:: [Maybie Char]

e3 = [Nothing,Just 'a']


2) Study the function definition below. Write down a type that Haskell
would agree was a valid type for that function.  (8 points)


null :: _________________________________________________

null xs = if xs==[] then True else False

----------------------------------------------------------------------------
Reasoning about programs

3) Prove the following by induction (use 'xs' as the induction variable)

init (xs ++ [x]) = xs

As you do your proof, justify each step by appealing to the line
number (in the comment following each clause). 

init[x] = []                              -- 1
init(x:xs) = x:(init xs)                 -- 2
[] ++ ys = ys                            -- 3
(x:xs) ++ ys = x:(xs ++ ys)              -- 4


------------------------
init (xs ++ [x]) = xs
1) []
2) (z:zs)
3) bottom

1)
init ([] ++ [x]) = []
init ([x]) = []
init (x:[]) = []
[]=[] (by 1)

2)
IH = init (zs ++ [x]) = zs

init ((z:zs) ++ [x]) = z:zs
init (z:(zs ++ [x])) = z:zs (by 4)
z:(init (zs ++ [x])) = z:zs (by 2)
z:zs = z:zs (IH)


3) 
init (bottom ++ [x]) = bottom
init bottom = bottom ( ++ pattern matches on left, doing that on bottom causes the thing become bottom)
bottom = bottom (Sae thing as above except for init)

-----------------------------

data Tree a - Tip a | Fork (Tree a) (Tree a) | LEaf

flip (Tip a) = Tip a
flip (Fork x y) = Fork (flip y) (flip x)
flip leaf = leaf

Law: flip (flip xs) = xs

F(xs) = fip(flip x) = xs

F(Tip x)
F(Fork x y) 
	Asume: F(x) and F(y) to prove F(fork x y)
IH flip (flip x) = x
	flip(flip y) = y

	flip (flip (frok x y)) = Fork x y
	flip (Fork (flip y) (flip x)) = Fork xy (definition of fork)
	Fork (flip(flipx)) (flip(flip(y)))
	Fork x y 					(IH)

F(Leaf)
F(Bottom)

----------------------------------------------------------------------------
Writing functions

-- Non recursive functions
4) Without using recursion give definitions for the following functions
consistent with the tests shown (12 points) (Use of prelude functions is
allowed)

span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
span (< 9) [1,2,3] == ([1,2,3],[])
span (< 0) [1,2,3] == ([],[1,2,3])

takeWhile, dropWhile





many 2 [1] ---> [1,1]
many 0 [1,2,3] ---> []
many 3 [4,5] ---> [4,5,4,5,4,5]
many 12 [] ---> []


cycle




-- Using Comprehensions
5) Without Using recursion, but using a comprehension and the reverse
function, write a function that behaves like the tests below. (10 points)

f [1,2,3,4] ---> [[4],[4,3],[4,3,2],[4,3,2,1]]    
f [] ---> []









-- Recursive programs with pattern matching
6) Study the data type for Music below. It is similar in form to the Music
data type from the HaskoreClassic except that 2 of the constructor functions
have been omitted (Trans, (:=:), and Instr). In the questions that follow
you only need consider the 4 constructors given.

data Music = Note Pitch Dur
           | Rest Dur
           | Music :+: Music
           | Tempo (Ratio Int) Music

A) Write the function, "shorter m n", which returns a Bool if playing the
music m lasts strictly less than the time duration n. Note the type of
shorter :: Music -> Dur -> Bool. Recall the type Dur is in the Num class,
and the Ord class, so all the usual arithmetic (+,-,*,/, etc.) and
comparison operators (==,>,<, etc.) are supported. The type (Ratio Int) is
also in the Num and Ord class, and be combined with Dur. I have left room on
the next page to write your answer. (12 points)
 


shorter:: Music -> Dur -> Bool





















----------------------------------------------------------------------------
Defining algebraic data types

7) Define an algebraic data type to represent ternary trees (trees where
internal nodes contain 3 sub trees) where no data is stored in the leaves,
and two values (of the same type) are stored in the internal nodes. (12
points)


data TernTree a = ....







8) Suggest one or more invariants for TernTree that might support fast search. (6 points)



9) Write a function search, that exploits the invariants you suggested
above. You may assume every TernTree used as an input to search meets your
invariants. (13 points)

search:: TernTree a -> a -> Bool
module Lecture4 where
	--Name: Andrey Byelogurov
	--Email: byelogurov@gmail.com
	-- 1 ---------------------------------------
	-- write a function that tests if its first
	-- argument is a element of its second.
	-- element 4 [2,6,7] --> False
	-- element 2 [,3,4,2,1] --> True
	-- element 'a' "cdef" --> False
	-- use pattern matching

	element:: Eq a => a -> [a] -> Bool
	element x [] = False
	element x (y:ys)
		| x == y = True
		| otherwise = element x ys


	-- 2 ----------------------------------------
	-- Write a function that enumerates all of the
	-- different ways in which a single value can
	-- be inserted into a list.  We will refer to
	-- this operator as inserts.  For example:
	--
	--  inserts '_' "fun"
	--    --> ["_fun","f_un","fu_n","fun_"]
	--
	--  inserts '_' "un"  ---> ["_un", "u_n", "un_"]
	--	inserts '_' "n" --> ["_n", "n_"]
	--  inserts 0 [1,2]
	--    --> [[0,1,2],[1,0,2],[1,2,0]]
	--

	inserts :: a -> [a] -> [[a]]
	inserts a [] = [[a]]
	inserts a (y:ys) = (a : (y:ys)) : map (y:) (inserts a ys)

	-- 3 ----------------------------------------
	-- Write a function that enumerates all of the
	-- permutations of a given input list.  For
	-- example:
	--
	--  perms [0,1]
	--   --> [[0,1],[1,0]]
	--  perms [0,1,2]
	--   --> [[0,1,2],[1,0,2],[1,2,0],[0,2,1],[2,0,1],[2,1,0]]
	--  perms [0,0,0]
	--   --> [[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0],[0,0,0]]
	--  perms "fun"
	--   --> ["fun","ufn","unf","fnu","nfu","nuf"]

	perms :: [a] -> [[a]]
	perms [] = []
	perms (x:xs) = [ x : n : []| n <- xs] : (perms xs)

	{-
		[0]
		[[0]]

		[0,1]
		[[0,1],[1,0]]

	-}


	-- 4 ----------------------------------------
	-- If you have are using a recent version of ghc,
	-- add an import Data.List declaration to the top
	-- of this module and then take a look at the
	-- function called permutations that this library
	-- provides.  How does it compare to the version
	-- of this function that you wrote in Part 2?
	--
	-- (Sorry, permutations is not part of the Haskell
	-- standard, and it is not included in the Hugs
	-- version of the Data.List library.)

	-- 5 ----------------------------------------
	-- Take a look at other functions in the Data.List
	-- library; do you understand what each of the
	-- functions does?  can you think of settings where
	-- they might be useful?  can you make sense of how
	-- these functions are defined?

	-- Done! ------------------------------------

	{-subsets:: [a] -> [[a]]
	subsets []= [[]]
	subsets (x:xs) = ys ++ map (x:) ys
		where ys = subsets xs


	fact:: Int -> Int
	fact 1 = 1
	fact (x) = x * fact (x - 1)

	filt :: (a -> Bool) -> [a] -> [a]
	filt p [] = []
	filt p (x:xs)
		| p x = x : rest
		| otherwise = rest
		where rest = filt p xs
	-}

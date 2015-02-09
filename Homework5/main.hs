module Homework5 where

--Name: Andrey Byelogurov
--Email: Byelogurov@gmail.com

import Test.HUnit
import Test.QuickCheck

{-
1) map f x ++ map f y = map f (x ++ y)

1 [] ++ [] = []
2 [] ++ xs = xs
3 [x] ++ [y] = [x, y]
4 map f [] = []
5 map f xs = [f (xs !! 0), f (xs !! 1), ..., f (xs !! n)]
--------------------------
Prove 3 things:

1) E([])		map f [] ++ map f y = map f y
2) 	Assuming:	E(zs)		map f zs ++ map f y == map f (zs ++ y)
	Prove:		E(z:zs)		map f (z:zs) ++ map f y == map f (z:zs ++ y)
3) E(bottom)	map f bottom ++ map f y = map f (bottom ++ y)

--------------------------

1) Prove map f [] ++ map f y = map f y:
		map f [] ++ map f y
		[] ++ map f y			-->	(by 4)
		map f y					--> (by 2)
				QED

2) Assuming:	E(zs)		map f zs ++ map f y == map f (zs ++ y)
   Prove:		E(z:zs)		map f (z:zs) ++ map f y == map f (z:zs ++ y)

	map f (z:zs) ++ map f y = (f z) : (map f zs) ++ map f y		--> (by 3)
	(f z) : (map f zs) ++ map f y = 

	--I really not sure where to go from here....

3) E(bottom)	map f bottom ++ map f y = map f (bottom ++ y)

======================

11) (xs ++ ys) ++ zs = xs ++ (ys ++ zs)

1 [] ++ [] = []
2 [] ++ xs = xs
--------------------------

1) E([])		([] ++ ys) ++ zs = [] ++ (ys ++ zs)
2)  Assuming:	(gs ++ ys) ++ zs = gs ++ (ys ++ zs)
	Prove:		((g:gs) ++ ys) ++ zs = (g:gs) ++ (ys ++ zs)
3) E(bottom)	(bottom ++ ys) ++ zs = bottom ++ (ys ++ zs)
--------------------------

1) ([] ++ ys) ++ zs 
	ys ++ zs			--> (by 2)
	(ys ++ zs)			--> (by parans)
	[] ++ (ys ++ zs)	--> (by 2 reverse)
		
2)	((g:gs) ++ ys) ++ zs
	g:(gs ++ ys) ++ zs		--> (by IH)
	g:(gs ++ ys ++ zs)		--> (by parans)
	(g:gs) ++ (ys ++ zs)	--> (by IH)

3)	(bottom ++ ys) ++ zs
	bottom ++ (ys ++ zs)	--> (by pattern matching?)


--Once again, im not sure if this proof is correct (I dbout it is)....
-}

-- ======================
-- Law 11 QuickTest 

prop_testLaw11 :: [Int] -> [Int] -> [Int] -> Bool
prop_testLaw11 xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

testLaw = quickCheck prop_testLaw11

-- ======================
-- Homework 3 boiler code

data Bit = O | I    deriving (Show, Eq)
type BinNum = [Bit]

toBinNum   :: Integer -> BinNum
toBinNum n
            | n == 0 = []
            | even n = O : toBinNum halfOfN
            | odd n  = I : toBinNum halfOfN
              where halfOfN = n `div` 2


fromBinNum :: BinNum -> Integer
fromBinNum []     = 0
fromBinNum (O:ds) = fromBinNum ds * 2
fromBinNum (I:ds) = 1 + fromBinNum ds * 2

inc :: BinNum -> BinNum
inc []  = [I]
inc (O:bs) = I : bs
inc (I:bs) = O : inc bs

add :: BinNum -> BinNum -> BinNum
add []     ds     = ds
add ds     []     = ds
add (O:ds) (I:es) = I : add ds es
add (O:ds) (O:es) = O : add ds es
add (I:ds) (O:es) = I : add ds es
add (I:ds) (I:es) = O : add (add ds [I]) es


mul :: BinNum -> BinNum -> BinNum
mul [] ds = []
mul ds [] = []
mul (a:as) (I:bs) = add (a:as) (O:(mul (a:as) bs))
mul (a:as) (O:bs) = add (zero) (O:(mul (a:as) bs))
    where zero = [O | n <- (a:as)]

-- ======================
-- Homework 3 add Test

at1 = TestCase (assertEqual "add [] []" (add [] []) [])
at2 = TestCase (assertEqual "add [I] [O]" (add [I] [O]) [I])
at3 = TestCase (assertEqual "add [I] [I]" (add [I] [I]) [O,I])
at4 = TestCase (assertEqual "add [I,O,I] [I,O,I]" (add [I,O,I] [I,O,I]) [O,I,O,I])
at5 = TestCase (assertEqual "add [I,I,I,I] [I]" (add [I,I,I,I] [I]) [O,O,O,O,I])

runAddTests = runTestTT $ TestList[at1, at2, at3, at4, at5]


-- ======================
-- Homework 3 new add function + tests

easyAdd :: BinNum -> BinNum -> BinNum
easyAdd b n = toBinNum $ (fromBinNum b) + (fromBinNum n)

eat1 = TestCase (assertEqual "easyAdd [] []" (easyAdd [] []) [])
eat2 = TestCase (assertEqual "easyAdd [I] [O]" (easyAdd [I] [O]) [I])
eat3 = TestCase (assertEqual "easyAdd [I] [I]" (easyAdd [I] [I]) [O,I])
eat4 = TestCase (assertEqual "easyAdd [I,O,I] [I,O,I]" (easyAdd [I,O,I] [I,O,I]) [O,I,O,I])
eat5 = TestCase (assertEqual "easyAdd [I,I,I,I] [I]" (easyAdd [I,I,I,I] [I]) [O,O,O,O,I])


runEasyAddTests = runTestTT $ TestList[eat1, eat2, eat3, eat4, eat5]

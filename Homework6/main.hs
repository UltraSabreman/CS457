{------------------------------------------------------------------------
CS457/557 Functional Languages                Type Classes (Homework 6)
-----------------------------------------------------------------------
Name of programmer: Andrey Byelogurov
Email to send comments to: byelogurov@gmail.com
-------------------------------------------------------------------------}
module Homework6 where

import Test.QuickCheck

{-
1) Define instances of Finite for the Int type (whose minimum and
maximum values can be obtained as (minBound::Int) and (maxBound::Int),
respectively), the product type (a, b) (assuming, of course that both
a and b are finite), and the sum type Either a b, which is defined in
the prelude as:

   data Either a b = Left a | Right b
Explain how your definitions work and use QuickCheck to verify that
your definitions satisfy the proposed law, at least on the basis of
some random tests.  [Note that a finite type can still have many
elements; you might want to think carefully about how you write your
instance definitions, especially the one for Int, to ensure that
testing does not take too long!]
-}

class Finite a where
  elems :: [a]
  size :: a -> Int
  size x = length (x:elems) - 1

instance Finite a => Finite (Maybe a) where
	elems = [ Nothing ] ++ [ Just x | x <- elems ]

instance Finite Bool where
  elems = [True, False]
  size x = 2
--Simply returns a list with Ture and false.

instance Finite Int where
  elems = 0 : [y | x <- [1..maxBound::Int], y <- [x,-x]]
  size x = x
--returns all ints. It's actulay kinda cool how this works,
--as it builds the list out from 0.

instance (Finite a, Finite b) => Finite (a, b) where
  elems = [ (x,y) | x <- elems, y <- elems]
  size (x, y) = (size x) * (size y)
--All possible sets of the two elems sets crossed together

instance (Finite a, Finite b) => Finite (Either a b) where
  elems = [Left x | x <- elems] ++ [Right x | x <-elems]
  size (Left x) = size x
  size (Right x) = size x
--A list of both lists combined.


prop_testInt :: Int -> Bool
prop_testInt x = elem x elems

prop_TestPow :: (Bool, Bool) -> Bool
prop_TestPow x = elem x elems

--prop_TestAdd :: (Either a b) -> Bool
--prop_TestAdd (Left x) = elem x elems
--Not sure how to build a test for this.....

{-
2)If a is a Finite type, then we can use elements :: [a] to obtain
the list of all values in the domain of a function of type a -> b.
Use this observation to define the following instances for displaying
functions (you may use whatever notation you prefer) and for testing
functions for equality:Ok

  instance (Finite a, Show a, Show b) => Show (a -> b) where
    show f = ...

  instance (Finite a, Eq b) => Eq (a -> b) where
    f == g  =  ...

You may, of course, define any auxiliary functions that you need to
make these definitions work as you would expect.
-}

instance (Finite a, Show a, Show b) => Show (a -> b) where
	show f =  concat [ show x ++ " => " ++ show (f x) ++ "|" | x <- elems]

instance (Finite a, Eq b) => Eq (a -> b) where
	f == g  = and [f x  ==  g x | x <- elems]


{-
3) Devise an algorithm for calculating a list of all functions of type
a -> b for any finite types a and b.  Show that this can be wrapped
up as an instance of the Finite type class:

  instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
    ...

Again, use QuickCheck to verify that your definition satisfies the law
specified in the introduction.
-}

instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
	elems = [ (\x -> y) | y <- elems] 

prop_TestFun :: (Finite a, Finite b, Eq a, Eq b) => (a -> b)-> Bool
prop_TestFun x = elem x elems

-- Frankly I have no idea what you mean by list of functions. This is my best
-- guess but I can't come up with a good way to test it.

{-
4) Suppose that we want to be able to calculate the number of elements
in any given finite type.  This can be accomplished by adding the
following lines to the end of the definition of the Finite class:

   size  :: a -> Integer
   size x = length (x:elements) - 1

Explain why the definition was written in this (rather odd) manner
instead of just writing:

   size  :: Integer
   size   = length elements

(If you're not sure why this wouldn't work, maybe you should enter it
into your code and see what happens ...)

Show that you can obtain considerably more efficient definitions for
size by overriding the implementation of size in specific instances of
the Finite class.
-}

-- Becuase we need to figure out what instance of elements we want first
-- and we do that by concating it with x. We also need to take one off the size
-- because of that as well.

-- For bools we can just return 2 cause there are only 2
-- for product it's much faster to simply multiply the sizes of the two sides
-- and for adition the same thing happens.


{-
5) Use QuickCheck to formulate and test some properties about size for
Maybe types, product types (pairs), and sum types (Either).  For
example, if the finite type t has n elements, then how many elements
would you expect to find in the type Maybe t?  How useful is QuickCheck
in cases like this?
-}

-- for Maybe
-- I'd expect to find N + 1, the set would contain nothing, and then just 0, just 1...just n

prop_sizeBool :: Bool -> Bool
prop_sizeBool b = (size (Just b)) == ((size b) + 1)

prop_sizeInt :: Int -> Bool
prop_sizeInt b = (size (Just b)) == ((size b) + 1)


prop_sizeProd :: (Bool, Bool) -> Bool
prop_sizeProd (x,y) = (size (x,y)) == ((size x) * (size y))

--For add, the size is just either the size of the left or right, not much to test.


--QuickCheck is ok here, because it gives us a really breif glance at weather or not
--our laws are good. Ofcourse further detailed testing would be required for things
--that aren't trivial, but it's "good enough"

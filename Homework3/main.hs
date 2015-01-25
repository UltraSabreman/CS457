----------------------------------------------------------------------
CS457/557 Functional Languages                              Homework 3
----------------------------------------------------------------------

Name of programmer:
Email to send comments to:

Due: At the start of class on January 26, 2015 on the class webct page.

Consider the following definition of a datatype of bits:

> data Bit = O | I    deriving Show

This datatype has two different values, written O and I, which we
will use to represent the bits 0 and 1.  We'll talk more about the
"deriving Show" part of this declaration in class soon, but for
now you can just treat it as an indication that we want to be able
to print out values of the Bit type, as in the following example:

Main> [I, O, I, O, I, I]
[I,O,I,O,I,I]
Main>

Now we can define a type of binary numbers:

> type BinNum = [Bit]

For convenience, we'll assume that the least significant bit is stored
at the head of the list so that, for example, [O, O, I] represents the
number 4 and [O, I, I, O, I, O] represents 22.

a) Define functions:

> toBinNum   :: Integer -> BinNum
> fromBinNum :: BinNum -> Integer

that convert backwards and forwards between Integers and their
corresponding BitNum representations.

You are welcome to construct your definitions of these functions
in any way, but if you are not sure where to start, then the
    following templates may give you some ideas:

toBinNum   :: Integer -> BinNum
toBinNum n  | n == 0 = []
            | even n = O : toBinNum halfOfN
            | odd n  = I : toBinNum halfOfN
            where halfOfN = n `div` 2

    > fromBinNum []     = ...
    > fromBinNum (O:ds) = ...
    > fromBinNum (I:ds) = ...

    If you prefer to approach these problems in a different way (for
    example, avoiding recursion), that's fine.

    b) Define a BinNum increment function

    > inc :: BinNum -> BinNum

    without using either toBinNum or fromBinNum, that satisfies
    the property:    inc . toBinNum = toBinNum . (1+)

    For example, inc [I,I,O,I,O,I] should yield [O,O,I,I,O,I]

    Hint: pattern matching and recursion should work together nicely
    for you here ...

    c) Define a function

    > add :: BinNum -> BinNum -> BinNum

    that computes the sum of its arguments. More formally, your add
    function should satisfy the following law (but your implementation
    should not make any use of Integer values):

    add x y = toBinNum (fromBinNum x + fromBinNum y)

    Hint: You might like to look for a definition that uses pattern
    matching on two arguments, together with a little bit of recursion.
    Something like the following might be a good start:

    >  add []     ds     = ...
    >  add ds     []     = ...
    >  add (O:ds) (e:es) = ...
    >  add (I:ds) (O:es) = ...
    >  add (I:ds) (I:es) = ...

    But this is not the only possible approach, so feel free to explore
    other options ...

    d) Define a function:

    > mul :: BinNum -> BinNum -> BinNum

    that computes the product of its arguments (without converting
    them to Integer values first).  Write a law to specify its behavior
    in relation to the (*) operator on Integer values.

    Hint: I'm not going to provide you with a template this time ---
    you've probably seen enough of those by now to be able to construct
    one for yourself. And don't forget that we've already defined some
    useful functions like inc and add for doing arithmetic on BinNum
    values; perhaps one of those will be useful to you here ...

    ----------------------------------------------------------------------

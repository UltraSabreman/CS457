-- Using implicit Types worksheet


--------------------------------------------------
-- Without consulting Hugs add a type declaration
-- to each of the named expressions


i1:: Integer  -- I have done the first one for you
i1 = 45

i2 = "123"

i3 = 45 <= i1

i4 = 'c'

i5 = ["abc","ok"]

i6 = head i5

i7 = tail "abc"  -- Recall a string is a shorthand for a list of Char

i8 = (True,4.5)

i9 = [i1,34]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = ("test", 5)

j2:: [Integer]
j2 = [0..10]

j3:: Char
j3 = 'd'


j4:: Double
j4 = 3.14159


j5:: (Integer,String,Integer,Char)
j5 = (3,"sdf",5,'g')

j6:: ([Char],(Bool,String))
j6 = ("sdfsd", (True, "sfsdf"))

j7:: [[Bool]]
j7 = [[True, False, True], [True, True, False]]

j8:: [(String,Bool)]
j8 = [("cat", True), ("Dog", False)]

Can you suggest suitable right hand sides to make valid laws
for each of the left hand sides given below?

Can you construct a simple example in each case to illustrate
how the corresponding law works in practice?

I've filled in the first law for you, and also pasted in an
expression from a Hugs session that illustrates the law in 
practice:

Composition:
------------

1) f . id             = f

Here's an example from Hugs with f = not

  Hugs> (not . id) True  ==  not True
  True
  Hugs> 

2) id . g             = g
	(id . not) False == not False

3) f . (g  . h)       =

Sections:
---------

4) (0+)               =

5) (n+) . (m+)        =

6) (*n) . (*m)        =

7) (xs++) . (ys++)    =


List processing:
----------------

8) (xs ++ ys) ++ zs   =

9) xs ++ []           =

10) length (zip xs ys) =

11) length . reverse   =

12) length . map f     =

13) length (xs ++ ys)  =

14) sum . map length   =

15) product . map product  =

16) sum . map sum      =

17) map f . reverse    =

18) map f . map g      =

19) map f xs ++ map f ys =

20) map f . concat     =

21) map f . filter (p . f) =

22) concat . map concat =

23) filter p . filter q =

24) concat . map (filter p) =

---- End!
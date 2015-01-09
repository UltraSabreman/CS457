module Homeowrk1 where 
	--Name: Andrey Byelogurov
	--Email: byelogurov@gmail.com

import Excell.hs

{-

"345" ( ['3','4','5'] )
[3,4,5]
[(3,100),(4,10),(5,1)]
[300,40,5]
345

-}


{-stringToInt x1 = (sum . map mult . powers . map decimal) x1
	
mult (x,y) = x * y
powers xs = zip [ 10^i | i <- [0..]] (reverse xs)
decimal  x
	| x <= '9' && x >= '0' = ord x - ord '0'
	| True = error ("Bad char in decimal "++[x])
-}

tab1 = col[1,2,3,4,5,6::Integer]

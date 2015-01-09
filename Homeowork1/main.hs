module Homework2 where
	--Name: Andrey Byelogurov
	--Email: byelogurov@gmail.com

import           Excell

tab1 = row['A','P','r','i','l',' ',' ']
tab2 = row["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]
tab3 = row["","","","1","2","3","4"]
tab4 = row["5","6","7","8","9","10","11"]
tab5 = row["12","13","14","15","16","17","18"]
tab6 = row["19","20","21","22","23","24","25"]
tab7 = row["26","27","28","29","30","",""]

tab8 = tab1 `above` tab2 `above` tab3 `above` tab4 `above` tab5 `above` tab6 `above` tab7
main = print tab8


{-

stringToInt x1 = (sum . map mult . powers . map decimal) x1

mult (x,y) = x * y
powers xs = zip [ 10^i | i <- [0..]] (reverse xs)
decimal  x
	| x <= '9' && x >= '0' = ord x - ord '0'
	| True = error ("Bad char in decimal "++[x])
-}

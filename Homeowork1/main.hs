module Homework2 where
	--Name: Andrey Byelogurov
	--Email: byelogurov@gmail.com

import           Excell

month  = row['A','p','r','i','l',' ',' '::Char]
days = row["Sun","Mon","Tue","Wed","Thu","Fri","Sat"::String]

week1 = blankRow 3 `beside` row[1,2,3,4::Integer]
week2 = row[5,6,7,8,9,10,11::Integer]
week3 = row[12,13,14,15,16,17,18::Integer]
week4 = row[19,20,21,22,23,24,25::Integer]
week5 = row[26,27,28,29,30::Integer]

final = month `above` days `above` week1 `above` week2 `above` week3 `above` week4 `above` week5
calendar = print final

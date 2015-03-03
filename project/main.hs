--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

module Homework5 where

fieldSize :: (Int, Int)
fieldSize = (3, 3)

testField :: [[Int]]
testField = [
			[0,1,0],
			[0,1,0],
			[0,0,0]
		]


properPos :: (Int, Int) -> (Int, Int)
properPos (x,y) = (nx, ny)
	where 
		ny = 
			if y < 0 then (snd fieldSize) + y 
			else if y >= snd fieldSize then y - (snd fieldSize)
			else y
		nx = 
			if x < 0 then (fst fieldSize) + x 
			else if x >= fst fieldSize then x - (fst fieldSize)
			else x

buildAdjList :: (Int, Int) -> [(Int, Int)]
buildAdjList (x, y) = [ properPos (j, k) | j <- [x - 1, x, x + 1], k <- [y - 1, y, y + 1], (j, k) /= (0,0)]

countLive :: (Int, Int) -> [[Int]] -> Int
countLive (x, y) field = sum [ field !! j !! k  | (j,k) <- buildAdjList (x, y)]

isCellAlive :: (Int, Int) -> [[Int]] -> Bool
isCellAlive (x, y) field = 
	if field !! x !! y == 1 then
		if countLive (x,y) field < 2 then False
		else if countLive (x,y) field == 2 || countLive (x,y) field == 3 then True
		else False
	else
		if countLive (x,y) field == 3 then True
		else False

computeField :: [[Int]] -> [[Int]]
computeField field = field

runGame :: Int -> [[Int]] -> [[Int]]
runGame 0 field = computeField field
runGame x field = runGame (x - 1) $ computeField field
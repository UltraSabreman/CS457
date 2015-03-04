--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

module Homework5 where

import Control.Concurrent
import System.Console.ANSI

fieldSize :: (Int, Int)
fieldSize = (10, 10)

testField :: [[Int]]
testField = [
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,1,0,0,0,0,0,0,0],
				[0,0,0,1,0,0,0,0,0,0],
				[0,1,1,1,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0],
				[0,0,0,0,0,0,0,0,0,0]
			]


nx :: Int -> Int
nx i 
	| i < 0 = nx $ (snd fieldSize) + i
	| i >= snd fieldSize = nx $ i - (snd fieldSize)
	| otherwise = i

{-properPosNoLoop :: (Int, Int) -> (Int, Int)
properPosNoLoop (x, y) = (nx, ny)
	where 
		nx 
			| x < 0 = 0
			| x >= fst fieldSize = fst fieldSize - 1
			| x
		ny
			| y < 0 = 0
			| y >= snd fieldSize = snd fieldSize - 1
			| y
-}

properPos :: (Int, Int) -> (Int, Int)
properPos (x,y) = (nx x, ny y)
	where 
		ny i 
			| i < 0 = ny $ (snd fieldSize) + i
			| i >= snd fieldSize = ny $ i - (snd fieldSize)
			| otherwise = i
		nx i 
			| i < 0 = nx $ (fst fieldSize) + i
			| i >= fst fieldSize = nx $ i - (fst fieldSize)
			| otherwise = i

buildAdjList :: (Int, Int) -> [(Int, Int)]
buildAdjList (x, y) = [ properPos (j, k) | k <- [y - 1, y, y + 1], j <- [x - 1, x, x + 1], (j, k) /= (x,y)]

countLive :: (Int, Int) -> [[Int]] -> Int
countLive (x, y) field = sum [ field !! k !! j  | (j,k) <- buildAdjList (x, y)]

isCellAlive :: (Int, Int) -> [[Int]] -> Int
isCellAlive (x, y) field = 
	if field !! y !! x == 1 then
		if countLive (x,y) field < 2 then 0
		else if countLive (x,y) field == 2 || countLive (x,y) field == 3 then 1
		else 0
	else
		if countLive (x,y) field == 3 then 1
		else 0

computeField :: [[Int]] -> [[Int]]
computeField field = [[isCellAlive (j, k) field | j <- [0..(fst fieldSize - 1)]] | k <- [0..(snd fieldSize - 1)]]

printField :: [[Int]] -> String
printField [] = ""
printField (x:xs) = (px x) ++ "\n" ++ printField xs
	where 
		px (l:ls)
			| l == 1 = "O " ++ px ls
			| otherwise = ". " ++ px ls
		px [] = ""

runGame :: Int -> [[Int]] -> IO ()
runGame 0 field = do {
				clearScreen;
				setCursorPosition 0 0;
				putStr $ printField field ++ "\n";
			}
runGame x field = do {
				clearScreen;
				setCursorPosition 0 0;
				putStr $ printField newField ++ "\n";	
				threadDelay 250000;
				runGame (x - 1) newField ;
			}
			where newField = computeField field


--Add color for output
--NEEED TESTS FOR GOOD GRADE!!!!!
main = do {
	setSGR [SetColor Foreground Vivid Red];
    putStr "Hello\n";
	setSGR [Reset];
}
		
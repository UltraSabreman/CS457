--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

module Project where

import Control.Concurrent
import System.Console.ANSI
import System.Random
import Data.Char

trueFieldSize :: [[Int]] -> (Int, Int)
trueFieldSize field = (length field, length $ field !! 0)

fieldSize :: [[Int]] -> (Int, Int)
fieldSize field = (fst size - 1, snd size - 1)
    where size = trueFieldSize field

testField :: [[Int]]
testField = [
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,1,0,0,0,0,0,0,0],
        [0,0,0,1,0,0,0,0,0,0],
        [0,1,1,1,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0]
      ]


randomField :: Int -> Int -> Int -> [[Int]]
randomField mx my seed = [[ l !! ((y * x) + x) | x <- [0..mx]] | y <- [0..my]]
  where l = randomRs (0, 1) (mkStdGen seed)
  



{-properPosNoLoop :: (Int, Int) -> (Int, Int)
properPosNoLoop (x, y) = (nx, ny)
  where 
    nx 
      | x < 0 = 0
      | x >= fst fieldSize = fst fieldSize - 1
      | otherwise x
    ny
      | y < 0 = 0
      | y >= snd fieldSize = snd fieldSize - 1
      | otherwise y
-}

properPos :: [[Int]] -> (Int, Int) -> (Int, Int)
properPos f (x,y) = (nx x, ny y)
  where 
    ny i 
      | i < 0 = ny $ (snd $ trueFieldSize f) + i
      | i >= (snd $ trueFieldSize f)  = ny $ i - (snd $ trueFieldSize f)
      | otherwise = i
    nx i 
      | i < 0 = nx $ (fst $ trueFieldSize f) + i
      | i >= (fst $ trueFieldSize f) = nx $ i - (fst $ trueFieldSize f)
      | otherwise = i

buildAdjList :: [[Int]] -> (Int, Int) -> [(Int, Int)]
buildAdjList f (x, y) = [ properPos f (j, k) | k <- [y - 1, y, y + 1], j <- [x - 1, x, x + 1], (j, k) /= (x,y)]

countLive :: (Int, Int) -> [[Int]] -> Int
countLive (x, y) field = sum [ field !! k !! j  | (j,k) <- buildAdjList field (x, y)]

isCellAlive :: (Int, Int) -> [[Int]] -> Int
isCellAlive (x, y) field =
  if field !! y !! x == 1 then
    if countLive (x,y) field == 2 || countLive (x,y) field == 3 then 1
    else 0
  else
    if countLive (x,y) field == 3 then 1
    else 0 

computeField :: [[Int]] -> [[Int]]
computeField field = [[isCellAlive (j, k) field | j <- [0..(fst $ fieldSize field)]] | k <- [0..(snd $ fieldSize field)]]

printField :: [[Int]] -> IO ()
printField [] = putStr ""
printField (x:xs) = do {
    (px x);
    putStr "\n";
    printField xs;
  }

  where 
    px (l:ls)
      | l == 1 = do {
        setSGR [SetColor Foreground Vivid Green];
        putStr "O ";
        px ls;
        setSGR [Reset];
      }
      | otherwise = do {
        setSGR [SetColor Foreground Vivid Red];
        putStr ". ";
        px ls;
        setSGR [Reset];
      }
    px [] = putStr ""

runGame :: Int -> [[Int]] -> IO ()
runGame 0 field = do {
        clearFromCursorToScreenBeginning ;
        setCursorPosition 0 0;
        printField field;
        putStr "\n";
      }
runGame x field = do {
        clearFromCursorToScreenBeginning ;
        setCursorPosition 0 0;
        printField newField; 
        putStr "\n";
        threadDelay 250000;
        runGame (x - 1) newField ;
      }
      where newField = computeField field


runRandomGame :: Int -> IO () 
runRandomGame steps = do {
    x <- randomRIO (0, 1);
    runGame steps $ randomField 20 20 x;
  }

--NEEED TESTS FOR GOOD GRADE!!!!!

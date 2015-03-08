--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

module Project where

import Control.Concurrent
import System.Console.ANSI
import System.Random
import System.IO
import System.Console.Haskeline
import Control.Monad.IO.Class

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
randomField mx my seed = [[ l !! ((y * x) + x) | x <- [0..(mx - 1)]] | y <- [0..(my - 1)]]
  where l = randomRs (0, 1) (mkStdGen seed)
  
nullField :: Int -> Int -> [[Int]]
nullField sx sy = [[ 0 | x <- [0..(sx - 1)]] | y <- [0..(sy - 1)]]


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


printNullField :: [[Int]] -> IO ()
printNullField field = do {
    setSGR [SetColor Foreground Vivid Red];
    printNullTable $ snd $ fieldSize field;
    setSGR [Reset];
  }
  where 
    printNullTable 0 = putStr ""
    printNullTable i = do { printNullRow $ fst $ fieldSize field; putStr "\n"; printNullTable (i - 1);}

    printNullRow 0 = putStr ""
    printNullRow i = do { putStr ". "; printNullRow (i - 1); }


printField2 :: [[Int]] -> [[Int]] -> Int -> Int -> IO ()
printField2 fOld fNew x y
  | x <= (fst $ fieldSize fNew) && y <= (snd $ fieldSize fNew) = do {
      if fOld !! y !! x /= fNew !! y !! x then
        if fNew !! y !! x == 1 then live else dead
      else
        return ();
      printField2 fOld fNew (x + 1) y;
    }
  | x > (fst $ fieldSize fNew) = printField2 fOld fNew 0 (y + 1)
  | y > (snd $ fieldSize fNew) = putStr ""

  where
    dead = do {
      setCursorPosition (y) (x * 2); --The * 2 is to make up for the " " when a char is printed.
      setSGR [SetColor Foreground Vivid Red];
      putStr ". ";
      setSGR [Reset];
    }
    live = do {
      setCursorPosition (y) (x * 2);
      setSGR [SetColor Foreground Vivid Green];
      putStr "O ";
      setSGR [Reset];
    }
    
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
        setCursorPosition 0 0;
        printField field;
        putStr "\n";
      }
runGame x field = 
  if theSame then do {
      cls;
      printField field;
      putStr "\n Stable field state achived! \n";
    }
  else do {
        setCursorPosition 0 0;
        printField2 field newField 0 0; 
        putStr "\n";
        threadDelay 250000;
        runGame (x - 1) newField ;
      }
      where 
        newField = computeField field
        theSame = field == newField



runRandomGame :: Int -> IO () 
runRandomGame steps = do {
    x <- randomRIO (0, 1);
    cls;
    printField $ randomField 20 20 x;
    threadDelay 250000;
    setCursorPosition 0 0;
    runGame steps $ randomField 20 20 x;
  }

runWithUser :: IO () 
runWithUser = do {
    cls;
    putStr "Enter the width of the field: ";
    w <- getLine;
    putStr "\nAnd the heigth: ";
    h <- getLine;
    cls;
    printField $ field w h;
    putStr "\nWASD: Move   Space: Toggle Cell    q: Run\n";
    setCursorPosition 0 0;
    showCursor;
    runInputT defaultSettings $ loop 0 0 $ field w h;
  }
  where 
    field :: String -> String -> [[Int]]
    field w h = nullField (read w :: Int) (read h :: Int)

    loop :: Int -> Int -> [[Int]] -> InputT IO ()
    loop x y f = do {

        liftIO $ setCursorPosition 0 0;
        liftIO $ printField f;
        outputStrLn "\nWASD: Move, Space: Toggle Cell, r: Run x Iterations, e: Step, q: quit\n";
        liftIO $ setCursorPosition x y;

        minput <- getInputChar "";
        clearPress x y f;
        case minput of
          Nothing -> return ()
          Just 'q' -> liftIO $ cls;

          Just 'r' -> do {
            liftIO $ setCursorPosition ((snd $ fieldSize f) + 2) 0;
            liftIO $ putStr "\nEnter the number of iterations to run: ";
            i <- (liftIO $ getLine);
            liftIO $ cls;
            liftIO $ printField f;
            liftIO $ threadDelay 250000;
            liftIO $ runGame (read i :: Int) f;
          }

          Just 'w' -> moveCur 'w' x y f
          Just 'a' -> moveCur 'a' x y f
          Just 's' -> moveCur 's' x y f
          Just 'd' -> moveCur 'd' x y f

          Just ' ' -> loop x y $ toggleCell y x f

          Just 'e' -> do { 
              liftIO $ setCursorPosition 0 0;
              liftIO $ printField2 f (computeField f) 0 0; 
              liftIO $ putStr "\n";
              liftIO $ threadDelay 250000;
              loop x y $ computeField f; 
            }

          Just input -> loop x y f
      }
    clearPress :: Int -> Int -> [[Int]] -> InputT IO() 
    clearPress x y f = 
      if f !! (quot y 2)!! x == 1 then 
        do {
          liftIO $ setSGR [SetColor Foreground Vivid Green];
          liftIO $ setCursorPosition x y;
          outputStr "O";
          liftIO $ setSGR [Reset];
        }
      else
        do {
          liftIO $ setSGR [SetColor Foreground Vivid Red];
          liftIO $ setCursorPosition x y;
          outputStr ".";
          liftIO $ setSGR [Reset];
        }

    moveCur :: Char -> Int -> Int -> [[Int]] -> InputT IO()
    moveCur c x y f = do {
      if      c == 'a' && y > 0 then                         loop x (y - 2) f;
      else if c == 'w' && x > 1 then                         loop (x - 1) y f;
      else if c == 'd' && y < ((snd $ fieldSize f) * 2) then loop x (y + 2) f;
      else if c == 's' && x < (fst $ fieldSize f) then       loop (x + 1) y f;
      else                                                   loop x y f;
    }

cls :: IO()
cls = do {
    clearScreen;
    setCursorPosition 0 0;
  }


toggleCell :: Int -> Int -> [[Int]] -> [[Int]]
toggleCell x 0 (r:rw) = (doRow x r) : rw
  where 
    doRow _ [] = []
    doRow 0 (i:is) = if i == 1 then 0:is else 1:is 
    doRow x (i:is) = i : (doRow (x - 2) is)
toggleCell x y (r:rw) = r : (toggleCell x (y - 1) rw)



--NEEED TESTS FOR GOOD GRADE!!!!!

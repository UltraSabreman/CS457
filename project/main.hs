--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

module Project where

import Control.Concurrent
import System.Console.ANSI
import System.Random
import System.IO
import System.Console.Haskeline
import Control.Monad.IO.Class
import Test.HUnit

{---------------------}
{- Utility Fucntions -}
{---------------------}

--Custom cell data type
data Cell = Alive | Dead deriving (Show, Eq)

--This gets the actual size of the field
--Used for calculations
trueFieldSize :: [[Cell]] -> (Int, Int)
trueFieldSize [] = (0, 0)
trueFieldSize [[]] = (0, 0)
trueFieldSize field = (length field, length $ field !! 0)

--This gets the size of the field -1 on both dimentions
--Used for iteration and limits
fieldSize :: [[Cell]] -> (Int, Int)
fieldSize [] = (0, 0)
fieldSize [[]] = (0, 0)
fieldSize field = (fst size - 1, snd size - 1)
    where size = trueFieldSize field

--Converts a cell to a bool
isAlive :: Cell -> Bool
isAlive Alive = True
isAlive _ = False

--Gets a certain cell from a field
getCell :: (Int, Int) -> [[Cell]] -> Cell
getCell (x, y) f = f !! y !! x

--Prints the cell at the cursour
printCell :: Cell -> IO ()
printCell cell = do {
    if cell == Dead then do {
      setSGR [SetColor Foreground Vivid Red];
      putStr ". ";
    }
    else do {
      setSGR [SetColor Foreground Vivid Green];
      putStr "O ";
    } ;
    setSGR [Reset];
  }

--Sleeps for 100ms
frameDelay :: IO ()
frameDelay = threadDelay 100000;

--This clears the screen
cls :: IO ()
cls = do {
    clearScreen;
    setCursorPosition 0 0;
  }

--This flips the given cell from dead to alive.
toggleCell :: (Int, Int) -> [[Cell]] -> [[Cell]]
toggleCell (x, 0) (r:rw) = (doRow x r) : rw
  where 
    doRow :: Int -> [Cell] -> [Cell]
    doRow _ [] = []
    doRow 0 (i:is) = if i == Alive then Dead:is else Alive:is 
    doRow x (i:is) = i : (doRow (x - 1) is)
toggleCell (x, y) (r:rw) = r : (toggleCell (x, y - 1) rw)


{-------------------------}
{- Computation Functions -}
{-------------------------}

--Generates a random field.
randomField :: (Int, Int) -> Int -> [[Cell]]
randomField (mx, my) seed = [[ num (x, y) | x <- [0..(mx - 1)]] | y <- [0..(my - 1)]]
  where 
    num :: (Int, Int) -> Cell
    num (x, y) = if (randomRs (0, 1::Int) $ mkStdGen seed) !! ((y * x) + x) == 1 
      then Alive
      else Dead
  
--Generates an empty field
nullField :: (Int, Int) -> [[Cell]]
nullField (sx, sy) = [[ Dead | x <- [0..(sx - 1)]] | y <- [0..(sy - 1)]]

--This wraps a number around a bound
wrappedPos :: [[Cell]] -> (Int, Int) -> (Int, Int)
wrappedPos f (x, y) = (x `modulo` width, y`modulo` height)
  where
    width = fst $ trueFieldSize f
    height = snd $ trueFieldSize f

    modulo :: Int -> Int -> Int
    modulo n m | r < 0     = r + m
           | otherwise = r
      where r = n `mod` m


--Builds a list of adjecent positions.
buildAdjList :: [[Cell]] -> (Int, Int) -> [(Int, Int)] 
buildAdjList f (x, y) = [ wrappedPos f (x + j, y + k) | k <- [-1..1], j <- [-1..1], (j, k) /= (0, 0)]

--Counts number of living cells next to given cell
countLive :: (Int, Int) -> [[Cell]] -> Int
countLive p field = length $ filter isAlive [ getCell pos field | pos <- buildAdjList field p]

--Figures out weather the curret cell is Alive or dead
computeCell :: (Int, Int) -> [[Cell]] -> Cell
computeCell pos field =
  if numLiving == 3 || (getCell pos field == Alive && numLiving == 2) 
    then Alive else Dead
  where numLiving = countLive pos field

--Runs one step of the simultation on the entire field
computeField :: [[Cell]] -> [[Cell]]
computeField field = [[computeCell (j, k) field | j <- [0..(fst $ fieldSize field)]] | k <- [0..(snd $ fieldSize field)]]

--This only prints the changed cells between the two provided fields
printFieldStep :: [[Cell]] -> [[Cell]] -> (Int, Int) -> IO ()
printFieldStep fOld fNew pos@(x,y)
  | x <= (fst $ fieldSize fNew) && y <= (snd $ fieldSize fNew) = do {
      if getCell pos fOld /= getCell pos fNew then draw else return ();
      printFieldStep fOld fNew (x + 1, y);
    }
  | x > (fst $ fieldSize fNew) = printFieldStep fOld fNew (0, y + 1)
  | y > (snd $ fieldSize fNew) = putStr ""

  where
    draw = do {
      setCursorPosition y (x * 2) ; --The * 2 is to make up for the " " when a char is printed.
      printCell $ getCell (x,y) fNew;
    }

    
--this prints the entire field
printField :: [[Cell]] -> IO ()
printField [] = putStr ""
printField (x:xs) = do {
    printRow x;
    putStr "\n";
    printField xs;
  }
  where 
    printRow :: [Cell] -> IO()
    printRow [] = putStr ""
    printRow (l:ls) = do {
      printCell l;
      printRow ls;
    }


{------------------}
{- Game Fucntions -}
{------------------}

--this runs the simulation for the provided number of steps
--using the provided field.
runGame :: Int -> [[Cell]] -> IO ()
runGame 0 field = do {
        setCursorPosition 0 0;
        printField field;
      }
runGame x field = 
  if theSame then do {
      cls;
      printField field;
      putStr "\n Stable field state achived! \n";
    }
  else do {
        setCursorPosition 0 0;
        printFieldStep field newField (0, 0); 
        putStr "\n";
        frameDelay;
        runGame (x - 1) newField ;
      }
      where 
        newField = computeField field
        theSame = field == newField

--this runs the simulations for the provied number of steps
--using a randomly generated field.
runRandomGame :: Int -> IO () 
runRandomGame steps = do {
    x <- randomRIO (0, 1);
    cls;
    printField $ randomField (20, 20) x;
    frameDelay;
    setCursorPosition 0 0;
    runGame steps $ randomField (20, 20) x;
  }

--this starts the main input loop and runs the simulation
--using user input.
runWithUserInput :: IO () 
runWithUserInput = do {
    cls;
    putStr "Enter the width of the field: ";
    w <- getLine;
    putStr "\nAnd the heigth: ";
    h <- getLine;
    cls;
    printField $ null w h;
    putStr "\nWASD: Move   Space: Toggle Cell    q: Run\n";
    setCursorPosition 0 0;
    showCursor;
    runInputT defaultSettings $ loop (0, 0) $ null w h;
  }
  where 
    --this simply creates a null field
    null :: String -> String -> [[Cell]]
    null w h = nullField (read w :: Int, read h :: Int)

    --this is the main input loop. I got the basic code from the haskeline page.
    loop :: (Int, Int) -> [[Cell]] -> InputT IO ()
    loop pos@(x, y) f = do {
        liftIO $ setCursorPosition 0 0;
        liftIO $ printField f;
        outputStrLn "\nWASD: Move, Space: Toggle Cell, r: Run x Iterations, e: Step, q: quit\n";
        --liftIO $ putStr $ show (x,y);
        liftIO $ setCursorPosition y (x * 2);

        minput <- getInputChar "";
        --x,y must be swapped due to field logic
        clearPress (x, y) f;

        case minput of
          Nothing -> return ()

          --R runs the simulation form here.
          --due to how computations are done, can't return here after finishing.
          Just 'r' -> do {
              liftIO $ setCursorPosition ((snd $ fieldSize f) + 2) 0;
              liftIO $ putStr "\nEnter the number of iterations to run: ";
              i <- (liftIO $ getLine);
              liftIO $ cls;
              liftIO $ printField f;
              liftIO $ frameDelay;
              liftIO $ runGame (read i :: Int) f;
            }
          --E steps the simulation one step, can be held down to run.
          Just 'e' -> do { 
              liftIO $ setCursorPosition 0 0;
              liftIO $ printFieldStep f (computeField f) (0, 0); 
              liftIO $ putStr "\n";
              liftIO $ frameDelay;
              loop pos $ computeField f; 
            }

          Just 'q' -> liftIO $ cls;       --Quits simulation
          Just 'w' -> moveCur 'w' pos f   --Move cursor up
          Just 'a' -> moveCur 'a' pos f   --Move cursor left
          Just 's' -> moveCur 's' pos f   --Move cursor down
          Just 'd' -> moveCur 'd' pos f   --Move cursor right

          --Toggle cell between dead/alive
          --X and y must be swapped because of how the field actually stores cells vs the display
          Just ' ' -> loop pos $ toggleCell (x, y) f 

          Just input -> loop pos f --All other input does nothing
      }
    
    --When a key is pressed in the main input loop, it's still written to screen.
    --this function simply over-writes that keystroke with what's supposed to be
    --in that field slot
    clearPress :: (Int, Int) -> [[Cell]] -> InputT IO() 
    clearPress (x, y) f = do {
        liftIO $ setCursorPosition y (x * 2);
        liftIO $ printCell $ getCell (x, y) f;
      }

    --this function actualy moves the physical cursor
    moveCur :: Char -> (Int, Int) -> [[Cell]] -> InputT IO()
    moveCur c (x, y) f = do {
      if      c == 'w' && y > 0 then                         loop (x, y - 1) f;
      else if c == 'a' && x > 0 then                         loop (x - 1, y) f;
      else if c == 's' && y < (snd $ fieldSize f) then       loop (x, y + 1) f;
      else if c == 'd' && x < (fst $ fieldSize f) then       loop (x + 1, y) f;
      else                                                   loop (x, y) f;
    }


{-----------}
{- Testing -}
{-----------}
--This makes a hardcoded test field.
--Usefull for testing individual functions
testField :: [[Cell]]
testField = map (map numToCell) $ [
        [0,1,0],
        [0,1,0],
        [0,1,0]
      ]
  where 
    numToCell x = if x == 1 then Alive else Dead

testField2 :: [[Cell]]
testField2 = map (map numToCell) $ [
        [0,0,0,0,0],
        [0,0,1,0,0],
        [0,0,1,0,0],
        [0,0,1,0,0],
        [0,0,0,0,0]
      ]
  where 
    numToCell x = if x == 1 then Alive else Dead

testField3 :: [[Cell]]
testField3 = map (map numToCell) $ [
        [0,0,0,0,0],
        [0,0,0,0,0],
        [0,1,1,1,0],
        [0,0,0,0,0],
        [0,0,0,0,0]
      ]
  where 
    numToCell x = if x == 1 then Alive else Dead

--trueFieldSize :: [[Cell]] -> (Int, Int)
test_trueFieldSize1 = TestCase (assertEqual "trueFieldSize testField" (trueFieldSize testField) (3,3))
test_trueFieldSize2 = TestCase (assertEqual "trueFieldSize [[]]" (trueFieldSize []) (0,0))
test_trueFieldSize3 = TestCase (assertEqual "trueFieldSize [[Alive, Dead],[Alive, Dead]]" (trueFieldSize [[Alive, Dead],[Alive, Dead]]) (2,2))
testList_trueFieldSize = TestList [test_trueFieldSize1, test_trueFieldSize2, test_trueFieldSize3]

--fieldSize :: [[Cell]] -> (Int, Int)
test_fieldSize1 = TestCase (assertEqual "fieldSize testField" (fieldSize testField) (2,2))
test_fieldSize2 = TestCase (assertEqual "fieldSize [[]]" (fieldSize []) (0,0))
test_fieldSize3 = TestCase (assertEqual "fieldSize [[Alive, Dead],[Alive, Dead]]" (fieldSize [[Alive, Dead],[Alive, Dead]]) (1,1))
testList_fieldSize = TestList [test_fieldSize1, test_fieldSize2, test_fieldSize3]

--getCell :: (Int, Int) -> [[Cell]] -> Cell
test_getCell1 = TestCase (assertEqual "getCell (0,0) [[Dead, Alive], [Alive, Dead]]" (getCell (0,0) [[Dead, Alive], [Alive, Dead]]) Dead)
test_getCell2 = TestCase (assertEqual "getCell (1,0) [[Dead, Alive], [Alive, Dead]]" (getCell (1,0) [[Dead, Alive], [Alive, Dead]]) Alive)
test_getCell3 = TestCase (assertEqual "getCell (0,0) testField" (getCell (0,0) testField) Dead)
test_getCell4 = TestCase (assertEqual "getCell (1,3) testField" (getCell (1,1) testField) Alive)
testList_getCell = TestList [test_getCell1, test_getCell2, test_getCell3, test_getCell4]

--isAlive :: Cell -> Bool
test_isAlive1 = TestCase (assertEqual "isAlive Alive" (isAlive Alive) True)
test_isAlive2 = TestCase (assertEqual "isAlive Dead" (isAlive Dead) False)
test_isAlive3 = TestCase (assertEqual "isAlive $ getCell (1,3) testField" (isAlive $ getCell (1,1) testField) True)
testList_isAlive = TestList [test_isAlive1, test_isAlive2, test_isAlive3]

--toggleCell :: (Int, Int) -> [[Cell]] -> [[Cell]]
test_toggleCell1 = TestCase (assertEqual "toggleCell (0,0) [[Dead, Dead], [Dead, Dead]]" (toggleCell (0,0) [[Dead, Dead], [Dead, Dead]]) [[Alive, Dead], [Dead, Dead]])
test_toggleCell2 = TestCase (assertEqual "toggleCell (0,0) [[Alive, Alive], [Alive, Alive]]" (toggleCell (0,0) [[Alive, Alive], [Alive, Alive]]) [[Dead, Alive], [Alive, Alive]])
test_toggleCell3 = TestCase (assertEqual "toggleCell (1,1) testField" (toggleCell (1,1) testField) ([[Dead, Alive, Dead], [Dead, Dead, Dead], [Dead, Alive, Dead]]))
test_toggleCell4 = TestCase (assertEqual "toggleCell (0,0) testField" (toggleCell (0,0) testField) [[Alive, Alive, Dead], [Dead, Alive, Dead], [Dead, Alive, Dead]])
testList_toggleCell = TestList [test_toggleCell1, test_toggleCell2, test_toggleCell3, test_toggleCell4]


--wrappedPos :: [[Cell]] -> (Int, Int) -> (Int, Int)
test_wrappedPos1 = TestCase (assertEqual "wrappedPos testField (0,0)" (wrappedPos testField (0,0)) (0,0))
test_wrappedPos2 = TestCase (assertEqual "wrappedPos testField (0,0)" (wrappedPos testField (0,0)) (0,0))
test_wrappedPos3 = TestCase (assertEqual "wrappedPos testField (-1,-1)" (wrappedPos testField (-1,-1)) (2,2))
test_wrappedPos4 = TestCase (assertEqual "wrappedPos testField (2,2)" (wrappedPos testField (3,3)) (0,0))
testList_wrappedPos = TestList [test_wrappedPos1, test_wrappedPos2, test_wrappedPos3, test_wrappedPos4]


--buildAdjList :: [[Cell]] -> (Int, Int) -> [(Int, Int)] 
test_buildAdjList1 = TestCase (assertEqual "buildAdjList testField2 (2,2)" (buildAdjList testField2 (2,2)) [(1,1),(2,1),(3,1),(1,2),(3,2),(1,3),(2,3),(3,3)])
test_buildAdjList2 = TestCase (assertEqual "buildAdjList testField2 (0,0)" (buildAdjList testField2 (0,0)) [(4,4),(0,4),(1,4),(4,0),(1,0),(4,1),(0,1),(1,1)])
testList_buildAdjList = TestList [test_buildAdjList1, test_buildAdjList2]

--countLive :: (Int, Int) -> [[Cell]] -> Int
test_countLive1 = TestCase (assertEqual "countLive (0,0) testField2" (countLive (0,0) testField2) 0)
test_countLive2 = TestCase (assertEqual "countLive (2,1) testField2" (countLive (2,1) testField2) 1)
test_countLive3 = TestCase (assertEqual "countLive (2,2) testField2" (countLive (2,2) testField2) 2)
testList_countLive = TestList [test_countLive1, test_countLive2, test_countLive3]

--computeCell :: (Int, Int) -> [[Cell]] -> Cell
test_computeCell1 = TestCase (assertEqual "computeCell (0,0) testField2" (computeCell (0,0) testField2) Dead)
test_computeCell2 = TestCase (assertEqual "computeCell (2,1) testField2" (computeCell (2,1) testField2) Dead)
test_computeCell3 = TestCase (assertEqual "computeCell (2,2) testField2" (computeCell (2,2) testField2) Alive)
test_computeCell4 = TestCase (assertEqual "computeCell (1,2) testField2" (computeCell (1,2) testField2) Alive)
testList_computeCell = TestList [test_computeCell1, test_computeCell2, test_computeCell3, test_computeCell4]


--computeField :: [[Cell]] -> [[Cell]]
test_computeField1 = TestCase (assertEqual "computeField [[Alive, Alive], [Dead, Dead]]" (computeField [[Alive, Alive], [Dead, Dead]]) [[Alive, Alive], [Dead, Dead]])
test_computeField2 = TestCase (assertEqual "computeField testField" (computeField testField) [[Alive, Alive, Alive], [Alive, Alive, Alive], [Alive, Alive, Alive]])
test_computeField3 = TestCase (assertEqual "computeField testField2" (computeField testField2) testField3)
testList_computeField = TestList [test_computeField1, test_computeField2, test_computeField3]


test_runAllTests = do {
  runTestTT testList_trueFieldSize;
  runTestTT testList_fieldSize;
  runTestTT testList_getCell;
  runTestTT testList_isAlive;
  runTestTT testList_toggleCell;
  runTestTT testList_wrappedPos;
  runTestTT testList_buildAdjList;
  runTestTT testList_countLive;
  runTestTT testList_computeCell;
  runTestTT testList_computeField;
}
module Main where

--Name: Andrey Byelogurov
--Email: byelogurov@gmail.com

import PPM6
import Data.Char

-- A mapping between chars and their "font" counterpart.
-- I know there's probably a better way to do this, and what not not, but this was 
-- the easiest and most straight-forward way I could come up with.
-- You get to experiance my custom bitmap font! 
--(this took me like 45 minutes so you better apprciate it :P)

-- Supported chars: a-z,.! and space
letter :: Char -> [(Int, Int)]
letter 'a' = [(-2,0), (-2,1), (-2, 2), (-1, 3), (-1, 4), (0, 5), (-1, 1), (-0, 1), (2,0)
              , (2,1), (2, 2), (1, 3), (1, 4), (0, 5), (1, 1), (0, 1)]
letter 'b' = [(-2,0), (-2, 1), (-2, 2), (-2, 3), (-2, 4), (-2, 5), (-1, 5), (0, 5), (1,5)
              , (2, 4), (1,3), (0, 3), (2, 2), (2, 1), (1, 0), (0, 0), (-1,0)]
letter 'c' = [(-2, 1), (-2, 2), (-2, 3), (-2,4), (-1,5), (0,5), (1,4), (-1,0), (0,0), (1,1)]
letter 'd' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,5), (0,5), (1,4), (1,3), (1,2), (1,1), (0,0), (-1,0)]
letter 'e' = [(-2,0), (-2, 1), (-2, 2), (-2, 3), (-2, 4), (-2, 5), (-1,5), (0,5), (1,5), (-1,3), (0,3), (-1,0), (0,0), (1,0)]
letter 'f' = [(-2,0), (-2, 1), (-2, 2), (-2, 3), (-2, 4), (-2, 5), (-1,5), (0,5), (1,5), (-1,3), (0,3)]
letter 'g' = [(-2, 1), (-2, 2), (-2, 3), (-2,4), (-1,5), (0,5), (1,4), (-1,0), (0,0), (1,1), (2,1), (2,0)]
letter 'h' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5),(-1,2),(0,2),(1,0), (1,1), (1,2), (1,3), (1,4), (1,5)]
letter 'i' = [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (-1,0), (1,0), (-1,5), (1,5)]
letter 'j' = [(-2,5), (-1,5), (0,5), (1,5), (0,4), (0,3), (0,2), (0,1), (-1,0), (-2,1)]
letter 'k' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,2), (0,1), (1,0), (-1,3), (0,4), (1,5)]
letter 'l' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,0), (0,0), (1,0)]
letter 'm' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,4), (0,3), (1,4), (2,0), (2,1), (2,2), (2,3), (2,4), (2,5)]
letter 'n' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,4), (-1,3), (0,2), (0,1), (1,0), (1,1), (1,2), (1,3), (1,4), (1,5)]
letter 'o' = [(-2,1), (-2,2), (-2,3), (-2,4), (-1,5), (0,5), (1,1), (1,2), (1,3), (1,4), (-1,0), (0,0)]
letter 'p' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,5), (0,5), (1,4), (1,3), (0,2), (-1,2)]
letter 'q' = [(2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (1,5), (0,5), (-1,4), (-1,3), (0,2), (1,2)]
letter 'r' = [(-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,5), (0,5), (1,4), (1,3), (0,2), (-1,2), (1,1), (1,0)]
letter 's' = [(-2,1), (-2,4), (-1,0), (-1,3), (-1,5), (0,0), (0,3), (0,5), (1,5), (1,2), (1,1)]
letter 't' = [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (-1,5), (1,5)]
letter 'u' = [(-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,0), (0,0), (1,1), (1,2), (1,3), (1,4), (1,5)]
letter 'v' = [(-2,5), (-2,4), (-2,3), (-2,2), (-1,1), (0,0), (1,1), (2,5), (2,4), (2,3), (2,2)]
letter 'w' = [(-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,0), (0,1), (1,0), (2,1), (2,2), (2,3), (2,4), (2,5)]
letter 'x' = [(-2,0), (-2,5), (-1,1), (-1,4), (0,2), (0,3), (2,0), (2,5), (1,1), (1,4)]
letter 'y' = [(-2,5), (-1,4), (0,2), (0,3), (0,1), (0,0), (2,5), (1,4)]
letter 'z' = [(-2, 5), (-1,5), (0,5), (1,5), (-2, 0), (-1,0), (0,0), (1,0), (-2,4), (-1,3), (0,2), (1,1)]
letter ',' = [(-2,0), (-1,1)]
letter '.' = [(-2,-2)]
letter '!' = [(-2,0), (-2,2), (-2,3), (-2,4), (-2,5)]
letter ' ' = []
letter c = []

--I couldn't use the point data because MapPixel takes Ints instead of doubles....

--Translates a list of points by the provided (x,y) pair
transPoints :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] 
transPoints t [] = []
transPoints (x,y) ((n,m):xs) = (n + x, m + y) : (transPoints (x,y) xs)

--Reflects a list of points over the X axis
refPoints :: [(Int, Int)] -> [(Int, Int)]
refPoints [] = []
refPoints ((x,y):xs) = (x, 0 - y) : refPoints xs

--This takes an offset, and a string array and returns a point list
--Baiscally this goes into the above map, retrievs the corisponding list of points
--The result is then translated
--It also does this again for the inverted version, except it reflects the points before translating.
--Finally it makes a recursive call and adds + to the horizontal offset (so that letters have proper spacing)
letterList :: (Int, Int) -> [Char] -> [(Int, Int)]
letterList t [] = []
letterList (x,y) (' ':cs) = letterList (x + 6, y) cs
letterList (x,y) (c:cs) = (transPoints (x, 200 - y) $ refPoints $ letter $ toLower c) ++ (transPoints (x,y) $ letter $ toLower c) ++ (letterList (x + 6, y) cs)



--Heads up, I use mapPixel in here instead of the other function
--This means that my display is flipped 90 degrees (x instead of y)
--That's why some of the calculations are strange looking

--This runs for every pixel in the image.
--If the pixel is in the list of pixels that are returned from the letterList function, it's collored green
--If they are in that list AND >100 in height, then they're colored light green.
--Other wise it draws the light blue and darker blue backgrounds
pixelPicker :: [Char] -> Int -> Int -> Colour
pixelPicker c x y 
          | (y,x) `elem` (letterList (5, 110) c) && x > 100 =  Colour 0.7 1 0.7
          | (y,x) `elem` (letterList (5, 110) c) =  green
          | x > 100 = Colour 0.3 0.3 1
          | x <= 100 = Colour 0.7 0.7 1
          
--Call this function with any string to have it print it out.
--The char limit (before it starts cutting of your string) is like ~30 chars
--Also, the longer the string the longer it will take, so be carefull
--(a full string will take ~4 minutes to run on my crappy laptop)
printString :: [Char] -> IO()
printString c = mapPixel "picture.ppm" (pixelPicker c) 200 200
 
 --For when this program is compiled.
main = printString "Haskell is Fun!"
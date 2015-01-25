{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Fixed version, 03-04-10, incorporating patch by Dan Brown


module Excell(ExData(..),Table(..),emptyTable

             ,row,rows,col,cols,blankCol, blankRow

             ,addCT,addTC,addRT,addTR,above, beside

             ,stack, lineUp

             ,export,showT)

      where





import CSV

import Data.List(transpose)



type Row = [String]

type Col = [String]

data Table = Tab Int Int [Row]



data Cell a = Blank | Full a



instance Show a => Show (Cell a) where

  show Blank = ""

  show (Full a) = show a

  

-----------------------------------------------

-- Excell Data Class



class Show t => ExData t where

  visualize:: t -> [String]

  visualize t = [show t]

  width:: t -> Int

  width = const 1





-- Easy instances with all default methods



instance ExData Integer where

instance ExData Bool where

instance ExData Double where  

instance ExData Int where



-- Harder instances  



instance ExData String where

  visualize x = [x]



instance ExData Char where

    visualize c = [[c]]

      

instance ExData a => ExData (Cell a) where

  visualize Blank = [""]

  visualize (Full x) = visualize x

  width ~(Full x) = width x



instance (ExData a,ExData b) => ExData (a,b) where

  visualize (x,y) = visualize x ++ visualize y

  width (x,y) = width x + width y



instance (ExData a,ExData b,ExData c) => ExData (a,b,c) where

  visualize (x,y,z) = visualize x ++ visualize y ++ visualize z

  width (x,y,z) = width x + width y + width z

  

instance (ExData a,ExData b,ExData c,ExData d) => ExData (a,b,c,d) where

  visualize (x,y,z,w) = visualize x ++ visualize y ++ visualize z ++ visualize w

  width (x,y,z,w) = width x + width y + width z  + width w

  

instance (ExData a,ExData b,ExData c,ExData d,ExData e) => ExData (a,b,c,d,e) where

  visualize (x,y,z,w,a) = visualize x ++ visualize y ++ visualize z ++ visualize w ++ visualize a

  width (x,y,z,w,a) = width x + width y + width z  + width w + width a

  



--instance (Show a,Show b,Show c,Show d,Show e,Show f) => Show (a,b,c,d,e,f) where

--  show(x,y,z,w,a,b) = "("++ show x++","++show y++","++show z++","++show w++","++show a++","++show b++")"



instance (ExData a,ExData b,ExData c,ExData d,ExData e,ExData f) => ExData (a,b,c,d,e,f) where

  visualize (x,y,z,w,a,b) = visualize x ++ visualize y ++ visualize z ++ 

                            visualize w ++ visualize a ++ visualize b

  width (x,y,z,w,a,b) = width x + width y + width z  + width w + width a + width b

  

-- instance (Show a,Show b,Show c,Show d,Show e,Show f,Show g) => Show (a,b,c,d,e,f,g) where

--  show(x,y,z,w,a,b,c) = "("++ show x++","++show y++","++show z++","++show w++","++show a++","++show b++","++show c++")"



instance (ExData a,ExData b,ExData c,ExData d,ExData e,ExData f,ExData g) => ExData (a,b,c,d,e,f,g) where

  visualize (x,y,z,w,a,b,c) = visualize x ++ visualize y ++ visualize z ++ 

                            visualize w ++ visualize a ++ visualize b ++ visualize c

  width (x,y,z,w,a,b,c) = width x + width y + width z  + width w + width a + width b + width c 

    

  

---------------------------------------------- 

-- Helper functions





pad :: Int -> [String] -> [String]

pad n xs = xs ++ replicate n ""





-- Zip together a list of m lists, each of length n 

-- listZip 3 [[2,3,4],[8,5,9]] --> [[2,8],[3,5],[4,9]]

listZip :: Int -> [[b]] -> [[b]]

listZip 0 zs = []
listZip n zs = (map head zs) : listZip (n-1) (map tl zs)
  where tl [x] = [x]
        tl (x:xs) = xs


fit :: Int -> String -> String

fit n xs | n==m = xs

         | n<m  = take n xs

         | n>m  = xs ++ replicate (n-m) ' '

  where m = length xs



---------------------------------------------------------

-- Tables with little or no content



emptyTable = Tab 0 0 []



blankCol n = col (replicate n "")

blankRow n = row (replicate n "")



---------------------------------------------------------

-- lists to tables





row:: ExData a => [a] -> Table

row x = rows [x]



rows :: ExData a => [[a]] -> Table

rows [] = Tab 0 0 []

rows xs = Tab numrows numcols (concat (map extend xs))

  where numrows = length xs

        numcols = maximum (map length xs)

        extend s = pads transpose (numcols - length s) s

        pads :: ExData a => ([[String]] -> [[String]]) -> Int -> [a] -> [[String]]

        pads transpose n xs =  map (++ replicate n "") (transpose (extend' (map visualize xs)))

        extend' s = s ++ replicate (numcols - length s) [""]





col:: ExData a => [a] -> Table

col xs = cols [xs]



cols :: ExData a => [[a]] -> Table

cols [] = Tab 0 0 []

cols xs = Tab numrows numcols y4

  where numcols = sum (map (width . head) xs)

        numrows = maximum (map length xs)

        y1 = map (extend . map visualize) xs

        y2 = transpose y1

        y3 = map concat y2

        y4 = {- map extend -} y3

        extend s = s ++ replicate (numcols - length s) [""]

  

        

addCT:: ExData a => [a] -> Table -> Table

addCT xs tab = beside (col xs) tab

 

addTC:: ExData a => Table -> [a] -> Table

addTC tab xs = beside tab (col xs)



addRT::  ExData a => [a] -> Table -> Table

addRT xs tab = above (row xs) tab



addTR::  ExData a => Table -> [a] -> Table

addTR tab xs = above tab (row xs)





--------------------------------------------------------

-- Tables to tables





above :: Table -> Table -> Table

(Tab i j xs) `above` (Tab n m ys) = Tab height width zs

  where width = max j m

        height = i + n

        zs = map (pad (width - j)) xs ++ map (pad (width - m)) ys







beside :: Table -> Table -> Table

(Tab i j xs) `beside` (Tab n m ys) = Tab height width (join xs ys)

  where width = j + m

        height = max i n

        join [] [] = []

        join (x:xs) (y:ys) = (x++y):(join xs ys)

        join [] (y:ys) = (replicate j "" ++ y):(join [] ys)

        join (x:xs) [] = (x ++ replicate m ""):(join xs [])





stack:: [Table] -> Table

stack [] = emptyTable

stack (t:ts) = above t (stack ts)



lineUp:: [Table] -> Table

lineUp [] = emptyTable

lineUp (t:ts) = beside t (lineUp ts)



------------------------------------------------



export :: String -> Table -> IO ()

export rootFileName (Tab _ _ xs) = writeFile (rootFileName++".csv") (printCSV xs)

  





showT :: Int -> Table -> String
showT n (Tab height width xs) = label ++ bar ++ concatMap row (padR width xs)
  where sizes = map (map length) xs
        colsizes = listZip width sizes
        colmax = map maximum colsizes
        f x = fit (min n x) (replicate x '-') ++ "+"
        bar = "\n+"++ concatMap f colmax ++ "\n"
        showRow (size,x) = fit (min n size) x ++ "|"
        row x = "|"++ concatMap showRow (zip colmax x) ++ bar
        label = show height ++ " rows by " ++ show width ++ " columns"


padR :: Int -> [[String]] -> [[String]]
padR n [] = []
padR n (x:xs) = (f x):padR n xs
  where f ys = ys ++ replicate (n - length ys) ""

instance Show Table where

  show x = showT 8 x

  

  

------------------------------------

module Main where

import PPM6

-- An example compiling this file. The -iDirectory, 
-- tells where to look for library functions
-- The -o file, says link and call the executable "file"

-- cd D:/work/sheard/Courses/CS457-557/web/Notes
-- ghc --make -iD:/work/sheard/Courses/CS457-557/web/Lib  DemoPPM.hs -o go.exe


-- THe main idea is to think of a "picture" on the 2-D plane
-- to be just a function from coordinate (Double,Double) to Colour

allred x y = red

go0 = mapDouble "Pics/allred.ppm" allred(-1,-1) (1,1) (100,100)

-------------------------------------------------------------        
-- Can we plot a circle?

close :: Double -> Double -> Double -> Bool
close epsilon x y = abs(x - y) <= epsilon
        
circle1 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else yellow 
              
go1 = mapDouble "Pics/circlePlain.ppm" 
                (circle1 0.05 4) (-3,-3) (3,3) (420,420) 

---------------------------------------------------
-- Plotting some reference points

origin epsilon c x y 
   | close epsilon x 0 || close epsilon y 0 = green
origin _ c _ _ = c 
           
circle2 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else origin (epsilon/3) yellow x y   

go3 = mapDouble "Pics/circleAxis.ppm" 
                (circle2 0.05 4) (-3,-3) (3,3) (420,420) 

------------------------------------------
-- Adding some extra stuff              

circle3 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else if close (5*epsilon) x 2.5 && close (5*epsilon) y 2.5
              then cyan
              else origin (epsilon/3) yellow x y                 


go4 = mapDouble "Pics/circleStuff.ppm" 
                (circle3 0.05 4) (-3,-3) (3,3) (420,420) 

---------------------------------------------------------------
-- Plotting any function of x. A graphing calculator

plotFun f name = mapDouble  name g (-3,-3) (3,3) (420,420) 
  where g x y = if close 0.05 (f x) y 
                   then red 
                   else origin 0.02 yellow x y
  
  
go5 = let f x = x*x in plotFun f "quadFun.ppm"
go6 = let f x = x*x*x - 2*x*x in plotFun f "cubicFun.ppm"

 
------------------------------------------------------------------
-- Plotting any list of points

plotPoints:: Double -> Colour -> Colour -> 
                      [Point] -> Double -> Double -> Colour
plotPoints epsilon background c xs x y =
  if any (near epsilon (x,y)) xs then c else background 


--------------------------------------------------------------
-- Exploring exponential growth

points :: [Point]
points = [ (double i,2.0 ** ((double i)/5.0)) | i <- [0..40] ] 


go7 = mapDouble "Pics/exponential.ppm" 
               (plotPoints 1.5 green yellow points) 
               (-1,-1) (41,260) (42*4*6,261*4)
  
---------------------------------------------------------
-- MandelBrot Fractals


next            :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot   :: Point -> [Point]
mandelbrot p  = iterate (next p) (0,0)

fairlyClose      :: Point -> Bool
fairlyClose (u,v) = (u*u + v*v) < 100

chooseColor        :: [color] -> [Point] -> color
chooseColor palette = 
      (palette!!) . length . take n . takeWhile fairlyClose
  where n = length palette - 1

fracImage :: (Point -> [Point]) -> [Colour] -> Point -> Colour
fracImage fractal palette = chooseColor palette . fractal

ppmPalette :: [Colour]
ppmPalette  = [ color ((2*i) `mod` (ppmMax+1)) i (ppmMax-i) 
              | i<-[0..ppmMax] ]
   where ppmMax      = 31 :: Int
         color r b g = Colour (fromIntegral r / 31) 
                              (fromIntegral b / 31) 
                              (fromIntegral g / 31)


testFrac x y = fracImage mandelbrot ppmPalette (x,y)

f3 = mapDouble "Pics/TimFrac.ppm"  testFrac (-2.25, -1.5) (0.75  , 1.5)    (600,400)
f4 = mapDouble "Pics/TimFrac2.ppm" testFrac (-1.25, -0.4) (-1.04 , -0.19)  (600,400)
f5 = mapDouble "Pics/TimFrac3.ppm" testFrac (-1.17, -0.34)(-1.128, -0.298) (600,400)

inMandelbrotSet  :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)

approxTest    :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

---------------------------------------------------------------
-- A checker board pattern

xor True False = True
xor False True = True
xor _ _ = False

checkers c1 c2 x y = if evenD x `xor` evenD y then c1 else c2

evenD :: Double -> Bool
evenD x = even(floor x)

f6 = mapDouble "Pics/check.ppm" (checkers red green) 
              (0, 0) (8,8) (400,400)


------------------------------------------------------------------
-- Can we generalize to something more useful?

              -- Use a point so we have a single arg
type BitMap = (Double,Double) -> Maybe(Colour)
                                 -- Might be undefined 
                                 -- for some points
over f g args = case f args of
                  Nothing -> g args
                  Just a -> Just a
                 
draw:: String -> Colour -> BitMap -> Point -> Point -> (Int,Int) -> IO ()
draw file color bitmap ll ur dim = mapDouble file map ll ur dim
  where fromJust (Just x) = x
        fromJust Nothing = color
        map x y = (fromJust.bitmap) (x,y) 

-------------------------------------------------------
-- Some simple BitMaps

-- Every where the same color
bkgrnd:: Colour -> BitMap
bkgrnd c (x,y) = Just c

-- A checkerboard BitMap
checks c1 c2 = Just . (uncurry (checkers c1 c2)) 

-- Draw the X and Y axis
axis (x,y) = if (close 0.05 x 0) || (close 0.05 y 0)
                then Just black
                else Nothing

-- Plot any function
plot :: Colour -> (Double -> Double) -> BitMap  
plot color f (x,y) = if close 0.05 (f x) y then Just color else Nothing

-- Draw circles of radius for each point in a list
dots:: Double -> Colour -> [Point] -> BitMap
dots radius color xs (x,y) =
  if any (near radius (x,y)) xs then Just color else Nothing
  
-- Draw a square
square color (lx,ly) (ux,uy) (x,y) = 
   if x >= lx && x <= ux && y >= ly && y <= uy 
      then Just color else Nothing

f7 = draw "Pics/graph.ppm" undefined pict (-4,-4) (4,4) (400,400)
  where pict = (plot green cubic) `over` 
               (dots 0.4 red [(0,3),(-2,1)]) `over` 
               axis `over` 
               (checks white yellow)

cubic x = x*x*x - 2*x*x

---------------------------------------------------
-- Can we do more?

layer :: [BitMap] -> BitMap
layer = foldl over (const Nothing) 


-- BitMap to BitMap functions
rotate:: Double -> BitMap -> BitMap
rotate theta f = g
  where g (x,y) = f(x*c+y*s,y*c - x*s) 
          where (s,c) = (sin theta,cos theta)
          
f8 = draw "Pics/graph.ppm" undefined (rotate (pi / 3) (layer ps)) (-4,-4) (4,4) (400,400)
  where ps = [plot green cubic
             , dots 0.4 red [(0,3),(-2,1)]
             ,rotate (pi /6)(square cyan (-3,1) (-1,3))
             ,plot blue witch
             , axis,checks white yellow]

witch x = 1.0 / (x*x + 1)

----------------------------------------------
-- Extensions?

oval :: Point -> Point -> Double -> BitMap
oval foci1 foci2 radius = undefined

translate :: Point -> BitMap -> BitMap
translate = undefined

resize :: Double -> BitMap -> BitMap
resize scale f = undefined

-- Other shapes, transformations?


actions = [go0,go1,go3,go4,go5,go6,go7,f3,f4,f5,f6,f7,f8]

main = mapM_ f (zip actions [0..])
  where f (act,n) = putStrLn ("Computing action "++show n) >> act
        
        
        
 
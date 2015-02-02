module PPM6 (Point,module Colour,double,dist,near,make_ppm, save_ppm, mapPixel, mapDouble) where
 
import Data.Word
import qualified Data.ByteString as BIN
import Colour
import Data.Array.IO
import System.IO(openBinaryFile,hClose,IOMode(..))
import Control.Monad(when)
----------------------------------------------------------------------
-- helper functions

type Point = (Double, Double)

double :: Int -> Double
double = fromIntegral

ratio (x,y) (u,v) = (x-u)/(y-v)

dist (x,y) (u,v) = sqrt((x-u)*(x-u) + (y-v)*(y-v))

near:: Double -> Point -> Point -> Bool
near eps (x,y) (u,v) = (a*a + b*b <= eps*eps)
   where a = (x-u) 
         b = (y-v)

-- select a small portion of the real-plane scaled and then offset      
select (lowerleft@(x,y)) (upperRight@(u,v)) percent delta = shift delta ((x,y),(x+dx,y+dy))
  where shift (deltaX,deltaY) ((x,y),(u,v)) = ((x+deltaX,y+deltaY),(u+deltaX,v+deltaY))
        dx = (u-x)*percent
        dy = (v-y)*percent

----------------------------------------------------------
-- dealing with Word8 entities
 
quant8 :: RealFrac n => n -> Word8
quant8 x = floor $ x * 0xFF
 
cquant8 :: Colour -> [Word8]
cquant8 (Colour r g b) = [quant8 r, quant8 g, quant8 b]


----------------------------------------------------------
-- List based bit-mapped interface

string_to_bin :: String -> BIN.ByteString
string_to_bin = BIN.pack . map (fromIntegral . fromEnum)
 
header :: [[Colour]] -> BIN.ByteString
header pss =
  let nx = length $ head pss
      ny = length        pss
  in  string_to_bin $ "P6\n" ++ show nx ++ " " ++ show ny ++ " 255\n"
 
body :: [[Colour]] -> BIN.ByteString
body pss = BIN.pack $ concatMap (cquant8 . cclip) $ concat pss
 
make_ppm :: [[Colour]] -> BIN.ByteString
make_ppm pss = BIN.append (header pss) (body pss)

 
save_ppm :: FilePath -> [[Colour]] -> IO ()
save_ppm f pss = BIN.writeFile f (make_ppm pss)

-----------------------------------------------------------------
-- Pixel function based bit-mapped interface

storeList :: (a -> Word8) -> [a] -> IOUArray Int Word8 -> Int -> IO Int
storeList inject s arr next = do { mapM put (zip [next .. count - 1] s); return count }
  where put (i,c) = writeArray arr i (inject c)
        count = next + length s
        
charToWord8 c = (fromIntegral(fromEnum c)) 


storePixel:: (Int -> Int -> a -> IO a) -> Int -> Int -> a -> IO a
storePixel perform rowSize colSize next = go 1 1 next
  where go i j next
           | i > rowSize = return next
           | j > colSize = go (i+1) 1 next
           | otherwise = do { n <- perform i j next; go i (j+1) n }
           
new:: Int -> IO (IOUArray Int Word8)
new n  = newArray (0,n) (fromIntegral 0)

make :: (Int -> Int -> Colour) -> Int -> Int -> IO (IOUArray Int Word8,Int)
make f nx ny = 
  do { let prefix = "P6\n" ++ show nx ++ " " ++ show ny ++ " 255\n"
           size = length prefix + 3*(nx * ny)
     ; arr <- new size
     ; next0 <- storeList charToWord8 prefix arr 0
     ; let perform i j next = storeList id (cquant8(cclip(f i j))) arr next
     ; next1 <- storePixel perform nx ny next0
     ; return (arr,next1)
     }

mapPixel file f nx ny = 
  do { (arr,size) <- make f nx ny 
     ; handle <- openBinaryFile file WriteMode
     ; hPutArray handle arr size
     ; hClose handle
     ; return ()
     }
     

-----------------------------------------------------------------
-- Real function based bit-mapped interface

-- Lift a real funtion of color to an IO action that writes that color to an Array

lift :: IOUArray Int Word8 -> (Double -> Double -> Colour) -> Double -> Double -> Int -> IO Int
lift arr f x y next = 
    case cclip(f x y) of
      (Colour r g b) -> do writeArray arr next     (quant8 r)
                           writeArray arr (next+1) (quant8 g)
                           writeArray arr (next+2) (quant8 b)
                           return (next +3)
 
-- Iterate over all the real valued coordinates, in the order 
-- that corresponds to the bit-map order.

iterDouble:: (Ord n,Num n) => (n -> n -> a -> IO a) -> (n,n,Int) -> (n,n,Int) -> a -> IO a
iterDouble perform 
           x@(xStart,xDelta,xCount) 
           y@(yStart,yDelta,yCount) 
           next = go xStart 1 yStart 1 next
  where go x i y j next
           | i > xCount =  go xStart 1 (y+yDelta) (j+1) next
           | j > yCount = return next
           | otherwise = do { n <- perform x y next; go (x+xDelta) (i+1) y j n }  


-- do we generate the points in the correct order?
testIterDouble = 
       do a <- iterDouble perf (-2,0.5,5) (5,-0.5,9) []
          putStrLn (show(length a))
          print a
   where perf x y ans = return(ans ++ [(x,y)])
           
        
-- Build an array of colors, open a file, and write the array to bit-mapped graphics file

mapDouble file perform (lowerleft@(x1,y1)) (upperright@(x2,y2)) (nx,ny) = 
   do { let prefix = "P6\n" ++ show nx ++ " " ++ show ny ++ " 255\n"
            size = length prefix + 3*(nx * ny)
      ; when (x1>=x2 || y1>=y2) 
           (error ("In a call to mapDouble, the Lower left corner "++show lowerleft++"\nis not below and to the left of the upper right corner "++show upperright))
     ; putStrLn "Creating prefix header"
     ; arr <- new size
        -- Store the PPM style 6 prefix
      ; next0 <- storeList charToWord8 prefix arr 0 
      ; putStrLn "Mapping the function"
      ; iterDouble (lift arr perform) (x1,rowDelta,nx) (y2,colDelta,ny) next0
      ; putStrLn ("Writing the ppm file: "++ file)
      ; handle <- openBinaryFile file WriteMode
      ; hPutArray handle arr size
      ; hClose handle
      ; return () }
  where rowDelta = (x2 - x1) / (fromIntegral (nx - 1) )
        colDelta = (y1 - y2) / (fromIntegral (ny - 1) )
 

--------------------------------
-- tests  


f 2 3 = white
f 5 1 = black
f _ 1 = red
f _ _ = blue

go = mapPixel "tim.ppm" f 10 10



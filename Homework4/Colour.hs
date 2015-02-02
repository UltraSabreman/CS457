module Colour where
 
data Colour = Colour {redPart, greenPart, bluePart :: Double} deriving (Eq, Show)
 
cmap :: (Double -> Double) -> Colour -> Colour
cmap f (Colour r g b) = Colour (f r) (f g) (f b)
 
czip :: (Double -> Double -> Double) -> Colour -> Colour -> Colour
czip f (Colour r1 g1 b1) (Colour r2 g2 b2) = Colour (f r1 r2) (f g1 g2) (f b1 b2)
 
cfold :: (Double -> Double -> Double) -> Colour -> Double
cfold f (Colour r g b) = r `f` g `f` b
 
cpromote :: Double -> Colour
cpromote x = Colour x x x
 
instance Num Colour where
  (+) = czip (+)
  (-) = czip (-)
  (*) = czip (*)
  negate = cmap negate
  abs    = cmap abs
  signum = cmap signum
  fromInteger x = cpromote (fromInteger x)
 
instance Fractional Colour where
  (/) = czip (/)
  recip = cmap recip
  fromRational x = cpromote (fromRational x)
 
clip :: (Num n, Ord n) => n -> n
clip n
  | n < 0 = 0
  | n > 1 = 1
  | otherwise = n
 
cclip :: Colour -> Colour
cclip = cmap clip



black = Colour 0 0 0
blue = Colour 0 0 1
green =Colour  0 0.5 0 
cyan = Colour 0 0.54296875 0.54296875
red = Colour 0.545098 0 0
magenta = Colour 0.54296875 0  0.54296875
yellow = Colour 1 1 0
white = Colour 1 1 1 
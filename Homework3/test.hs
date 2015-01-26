module Homework3 where

data Bit = O | I    deriving Show
type BinNum = [Bit]

toBinNum   :: Integer -> BinNum
toBinNum n
            | n == 0 = []
            | even n = O : toBinNum halfOfN
            | odd n  = I : toBinNum halfOfN
              where halfOfN = n `div` 2


fromBinNum :: BinNum -> Integer
fromBinNum []     = 0
fromBinNum (O:ds) = fromBinNum ds * 2
fromBinNum (I:ds) = 1 + fromBinNum ds * 2

inc :: BinNum -> BinNum
inc []  = [I]
inc (O:bs) = I : bs
inc (I:bs) = O : inc bs

add :: BinNum -> BinNum -> BinNum
add []     ds     = ds
add ds     []     = ds
add (O:ds) (I:es) = I : add ds es
add (O:ds) (O:es) = O : add ds es
add (I:ds) (O:es) = I : add ds es
add (I:ds) (I:es) = O : add (add ds [I]) es


mul :: BinNum -> BinNum -> BinNum
mul [] ds = []
mul ds [] = []
mul (a:as) (I:bs) = add (a:as) (O:(mul (a:as) bs))
mul (a:as) (O:bs) = add (zero) (O:(mul (a:as) bs))
    where zero = [O | n <- (a:as)]
{-
         1 0 1 1   (A)
       × 1 0 1 0   (B)
       ---------
         0 0 0 0   ← Corresponds to the rightmost 'zero' in B
+      1 0 1 1     ← Corresponds to the next 'one' in B
+    0 0 0 0
+  1 0 1 1
---------------
= 1 1 0 1 1 1 0
-}

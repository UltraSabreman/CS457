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
inc [O] = [I]
inc (O:ds) = I : inc ds
inc (I:ds) = O : inc ds

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
fromBinNum (O:ds) = 1 + fromBinNum ds
fromBinNum (I:ds) = 2 + fromBinNum ds

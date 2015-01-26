----------------------------------------------------------------------
CS457/557 Functional Languages                              Homework 3
----------------------------------------------------------------------

Name: Andrey Byelogurov
Email: byelogurov@gmail.com

a)
    toBinNum   :: Integer -> BinNum
    toBinNum n  | n == 0 = []
                | even n = O : toBinNum halfOfN
                | odd n  = I : toBinNum halfOfN
                where halfOfN = n `div` 2

    fromBinNum :: BinNum -> Integer
    fromBinNum []     = 0
    fromBinNum (O:ds) = fromBinNum ds * 2
    fromBinNum (I:ds) = 1 + fromBinNum ds * 2


b)
    inc :: BinNum -> BinNum
    inc []  = [I]
    inc (O:bs) = I : bs
    inc (I:bs) = O : inc bs

c)
    add :: BinNum -> BinNum -> BinNum
    add []     ds     = ds
    add ds     []     = ds
    add (O:ds) (I:es) = I : add ds es
    add (O:ds) (O:es) = O : add ds es
    add (I:ds) (O:es) = I : add ds es
    add (I:ds) (I:es) = O : add (add ds [I]) es


d)
    mul :: BinNum -> BinNum -> BinNum
    mul [] ds = []
    mul ds [] = []
    mul (a:as) (I:bs) = add (a:as) (O:(mul (a:as) bs))
    mul (a:as) (O:bs) = add (zero) (O:(mul (a:as) bs))
    where zero = [O | n <- (a:as)]
    ----------------------------------------------------------------------

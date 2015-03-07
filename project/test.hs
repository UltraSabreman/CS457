{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
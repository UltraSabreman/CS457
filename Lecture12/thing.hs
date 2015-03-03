{- # LANGUAGE MultiParamTypeClasses, FlexibleInstances # -}
module MonadInterpWorksheet where

data Store s x = St(s -> (x,s))

type Map = [(String,Value)]

-- Assumption that the list is a set 
-- Order doesn't matter, and no duplicates

--------------------------------------
-- Find a value in the Map

find :: Eq a => a -> [(a,b)] -> b
find nm pairs = head [ v | (n,v) <- pairs, n==nm]

-- Update a value in a Map

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update nm value pairs = (nm,value) : [ (n,v) | (n,v) <- pairs, n /= nm ]

-----------------------------------------------
-- Here is a new language with variables and assignments

data T5 = Add5 T5 T5
        | Sub5 T5 T5
        | Mult5 T5 T5
        | Int5 Int
        | Var5 String
        | Assign5 String  T5


-- Write an evaluator for the language T5
-- write it in functional style (that is don't use monads)
-- I started it for you.

type Value = Int

eval5a :: T5 -> Store Map Value
eval5a (Add5 x y) =
  St(\s-> let St f = eval5a x
              St g = eval5a y
              (x',s1) = f s
              (y',s2) = g s1
          in(x'+y',s2))
eval5a (Sub5 x y) = 
  St(\s-> let St f = eval5a x
              St g = eval5a y
              (x',s1) = f s
              (y',s2) = g s1
          in(x'-y',s2))

eval5a (Mult5 x y) = undefined
eval5a (Int5 n) = undefined
eval5a (Var5 s) = undefined
eval5a (Assign5 nm x) = undefined

-----------------------------------------------
-- Now write the operators on the Store monad


-- Given a name "x", "getStore x" returns a 
-- Store computation, that when run returns 
-- the value associated in the Map 
-- with the name given as input. 

getStore :: String -> (Store Map Value)
getStore nm = St h
  where h s = undefined

-- Given a name "x", and a new value "v" 
-- "putStore x v" returns a Store computation, 
-- that when it runs returns unit, but updates
-- the map so "x" is now mapped to "v"

putStore :: String -> Value -> (Store Map ())
putStore nm n = undefined
  
-------------------------------------------------------
-- Write an evaluator for the language T5 but
-- this time use use monads and the "do" syntax
-- Hint. use the operators on the Store Monad
-- I started it for you.
          
eval5 :: T5 -> Store Map Value
eval5 (Add5 x y) = 
      do {x' <- eval5 x; y' <- eval5 y; return (x' + y')}
eval5 (Sub5 x y) = undefined
eval5 (Mult5 x y) = undefined
eval5 (Int5 n) = undefined
eval5 (Var5 s) = undefined 
eval5 (Assign5 s x) = undefined


------------------------------
instance Monad (Store s) where
  return x = St(\ s -> (x,s))
  (>>=) (St f) g = St h
    where h s1 = g' s2 where (x,s2) = f s1
                             St g' = g x
                 
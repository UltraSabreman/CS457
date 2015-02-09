module Lect7 where

data BinTree a = Leaf a	| BinTree a :^: BinTree a deriving Show

example :: BinTree Int
example = l :^: r
	where 
		l = p :^: q
		r = s :^: t
		p = Leaf 1 :^: t
		q = s :^: Leaf 2
		s = Leaf 3 :^: Leaf 4
		t = Leaf 5 :^: Leaf 6

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (l :^: r) = mapTree f l :^: mapTree f r

type Path = [Int]
type NodeID = String

showPath	:: Path -> NodeID
showPath p 	= "\"" ++ show p ++ "\""

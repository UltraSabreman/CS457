


module RowYourBoat where
import Data.Char
import Music




data NULL
data Tree = NULL | Node Tree Char Tree deriving(Show)


insert :: Char -> Tree -> Tree
insert c NULL = Node NULL c NULL
insert c (Node l d r) 
	| ord c < ord d = Node (insert c l) d r
	| otherwise = Node l d (insert c r)

strToTree :: [Char] -> Tree
strToTree [] = NULL
strToTree (x:xs) = insert x $ strToTree xs

fuck :: Char -> Int
fuck c = ord c `mod` 14

{-treeToMusic :: Tree -> Int -> [Music] 
treeToMusic NULL acc = []
treeToMusic (Node l d r) acc = (treeToMusic l (acc - 1)) ++ [(fromAbsolute $ fuck d) acc en []] ++ (treeToMusic r (acc + 1))-}

noteNum 	:: PitchClass -> Int
noteNum x 	= case x of
	Cf -> -1; C ->  0; Cs -> 1
	Df ->  1; D ->  2; Ds -> 3
	Ef ->  3; E ->  4; Es -> 5
	Ff ->  4; F ->  5; Fs -> 6
	Gf ->  6; G ->  7; Gs -> 8
	Af ->  8; A ->  9; As -> 10
	Bf -> 10; B -> 11; Bs -> 12


toAbsolute     :: Pitch -> Int
toAbsolute (p,o)  = noteNum p + 12 * o


fromAbsolute   :: Int -> Pitch
fromAbsolute x    = let (a,b) = quotRem x 12
		in (notes !! b,a)
		where notes = [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]

--ultraTest = do { writeMidi (line (treeToMusic (strToTree "Andrey wat dis carp") 5))  "Music/wat"; }

{-tinsert :: (Ord a) =>  Tree a -> a -> Tree a 
tinsert Empty a         = Branch a Empty Empty
tinsert (Branch a left right) b
    | b == a = Branch a left right
    | b < a = Branch a (tinsert left b) right
    | b > a = Branch a left (tinsert right b)-}



{-row:: Music
row =   line [c 4 qn [], c 4 qn [], c 4 en [], d 4 en [], e 4 qn []] :+:
        line [e 4 en [], d 4 en [], e 4 en [], f 4 en [], g 4 hn []] :+:
        line [c 5 sn [], c 5 sn [], c 5 sn [], g 4 sn [], g 4 sn [], g 4 sn [],
        e 4 sn [], e 4 sn [], e 4 sn [], c 4 sn [], c 4 sn [], c 4 sn []] :+:
        line [g 4 en [], f 4 en [], e 4 en [], d 4 en [], c 4 hn []]



main =  do { writeMidi row "Music/row"; } -}

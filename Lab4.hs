polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

newtype CylindrycalCoord3 a = MkCylindrycalCoord3(a,a,a) deriving (Show)
newtype CartesianCoord3 a = MkCartesianCoord3(a,a,a) deriving (Show)

cartesianToCylindrycal'' :: (Floating a, Ord a) => CartesianCoord3 a -> CylindrycalCoord3 a
cartesianToCylindrycal'' (MkCartesianCoord3 (x,y,z)) = MkCylindrycalCoord3 (p*cos phi,p*sin phi,z) 
                        where p = sqrt(x^2+y^2)
                              phi | x == 0 && y == 0 = 0
                                  | x >= 0 = asin (y/p)
                                  | x > 0 = asin (y/x)
                                  | otherwise = -asin(y/p) + pi


--Wywolanie: let c = cartesianToCylindrycal'' $MkCartesianCoord3 (1,1,1) bo tam jest pattern matching a nie tworenie nowego

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) =
 "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfo' -> String
personInfoToString' (nm,snm,addr) = "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

------------------------------------------------------------------------------------------------
-- product type example (one constructor)
-- data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

-- xCoord :: CartInt2DVec -> Int
-- xCoord (MkCartInt2DVec x _) = x

-- yCoord :: CartInt2DVec -> Int
-- yCoord (MkCartInt2DVec _ y) = y

-- data Cart2DVec' a = MkCart2DVec' a a

-- xCoord' :: Cart2DVec' a -> a
-- xCoord' (MkCart2DVec' x _) = x

-- yCoord' :: Cart2DVec' a -> a
-- yCoord' (MkCart2DVec' _ y) = y --dowolny typ

-- *Main> :t xCoord' $MkCart2DVec' 5 10.0
-- xCoord' $MkCart2DVec' 5 10.0 :: Fractional a => a
-- Haskell uogolnia typ podczas uzywania konstruktora

--data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y

--Odwrotnie tez dziala ;)

--Dwa sposoby wywolania:
--ghci> xCoord'' $ MkCart2DVec'' {x=1, y=2}
--ghci> xCoord'' $ MkCart2DVec'' 1 2

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue | White | Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

data Cart3D a = MkCart3D {x::a,y::a,z::a}

xCoord3D :: Cart3D a -> a
xCoord3D (MkCart3D xval _ _) = xval

yCoord3D :: Cart3D a -> a
yCoord3D (MkCart3D _ yval _) = yval

zCoord3D :: Cart3D a -> a
zCoord3D (MkCart3D _ _ zval ) = zval

data Cart3DVec a = Cart3DVec a a a

xCoord3D' :: Cart3DVec a -> a
xCoord3D' (Cart3DVec xval _ _) = xval

yCoord3D' :: Cart3DVec a -> a
yCoord3D' (Cart3DVec _ yval _) = yval

zCoord3D' :: Cart3DVec a -> a
zCoord3D' (Cart3DVec _ _ zval ) = zval

data Cart3D'' = Cart3D'' Int Int Int

xCoord3D'' :: Cart3D'' -> Int
xCoord3D'' (Cart3D'' xval _ _) = xval





data Cart2D a = MkCart2D {
    x2D :: Double,
    y2D :: Double
} deriving Show

data Polar2D a = MkPolar2D{
    r2D :: Double,
    phi :: Double
} deriving Show

data Cart2D2 = MkCart2D2 Double Double

data Polar2D2 = MkPolar2D2 Double Double

printCart2D :: (Show a) => Cart2D a -> String
printCart2D (MkCart2D {x2D=xVal2,y2D=yVal2}) = "x coord: " ++ show xVal2 ++ " y coord " ++ show yVal2

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle a b) = a*b

data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving Show

rootValue :: Tree a -> a
rootValue (EmptyT) = error "Empty Tree"
rootValue (Node n (lt) (rt)) = n
--let c = (Node 10) EmptyT EmptyT

data TrafficLights = Red2 | Yellow | Green

actionFor::TrafficLights -> String
actionFor Red2 = "Stop"
actionFor Yellow = "Watch"
actionFor Green = "Go"

--actionFor Red2..

--------------------------------------------------------------------------------------------------
data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving Show

instance Eq a => Eq (BinTree a) where
    (==) (NodeBT a lt rt) (NodeBT b lt2 rt2) = (a == b) && (lt == lt2) && (rt == rt2)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

--ghci> sumBinTree EmptyBT
--ghci> sumBinTree $ NodeBT 1 EmptyBT EmptyBT
--ghci> sumBinTree (NodeBT 1 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT))

data Expr a = Lit a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Sub e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Mul e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

--data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a)

depthOfBT :: BinTree a -> Int --ok
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

flattenBT :: BinTree a -> [a] --ok
flattenBT EmptyBT = []
flattenBT (NodeBT n lt rt) = (flattenBT lt)++[n]++(flattenBT rt)

mapBT :: (a -> b) -> BinTree a -> BinTree b --ok
mapBT f EmptyBT = EmptyBT   
mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT f lt) (mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a --ok
insert x EmptyBT = NodeBT x (EmptyBT) (EmptyBT)
insert x (NodeBT n lt rt) | x < n = NodeBT n (insert x lt) rt
                          | otherwise = NodeBT n lt (insert x rt) 

list2BST :: Ord a => [a] -> BinTree a --nie dziala
list2BST x = loop x EmptyBT
            where loop [] a = a
                  loop (x:xs) a = loop xs (insert x a)

printTree :: Show a => BinTree a -> String
printTree EmptyBT = ""
printTree (NodeBT a lt rt) = show a ++" "++ (printTree lt)++" " ++ (printTree rt)

occurs :: Eq a => a -> BinTree a -> Int
occurs _ EmptyBT = 0
occurs x (NodeBT n lt rt) | x == n = 1 + (occurs x lt) + (occurs x rt)
                          | otherwise = (occurs x lt) + (occurs x rt)

elemOf :: Eq a => a -> BinTree a -> Bool
elemOf _ EmptyBT = False
elemOf x (NodeBT n lt rt) | x == n = True
                          | otherwise = (elemOf x lt) || (elemOf x rt)

reflect :: BinTree a -> BinTree a -- 'odbicie lustrzane' drzewa binarnego
reflect EmptyBT = EmptyBT
reflect (NodeBT n lt rt) = NodeBT n (reflect rt) (reflect lt)

-- minElemOf :: Ord a => BinTree a -> a
-- minElemOf EmptyBT = error "Empty Tree"
-- minElemOf (NodeBT n lt rt) = loop n lt
--                             where loop minVal EmptyBT = minVal
--                                   loop minVal (NodeBT x lt rt) = loop x lt

minElemOf :: Ord a => BinTree a -> a 
minElemOf EmptyBT = error "Empty Tree"
minElemOf (NodeBT n EmptyBT rt) = n
minElemOf (NodeBT n lt rt ) = minElemOf lt 

data Expr2 a = Lit2 a | Expr2 a :+: Expr2 a | Expr2 a :*: Expr2 a | Expr2 a :-: Expr2 a deriving Show

show2 :: Show a => Expr2 a -> String
show2 (Lit2 a) = show a
show2 (a :+: b) = "(" ++ (show2 a) ++ "+" ++ (show2 b) ++")"
show2 (a :*: b) = "(" ++ (show2 a) ++ "*" ++ (show2 b) ++")"
show2 (a :-: b) = "(" ++ (show2 a) ++ "-" ++ (show2 b) ++")"

--Main> show2 ((Lit2 2) :*: (Lit2 3))
-- #TODO
-- data Expr a = Lit a |
-- Op Ops (Expr a) (Expr a) |
-- If (BExpr a) (Expr a) (Expr a)

-- data Ops = Add | Sub | Mul

-- data BExpr a = BoolLit Bool |
--  And (BExpr a) (BExpr a) |
--  Or (BExpr a) (BExpr a) |
--  Not (BExpr a) |
--  Equal (Expr a) (Expr a) |
--  Greater (Expr a) (Expr a)



------------------------------------------------------------------------------
--Zadanie 5
data MyInt = MkMyInt Int
--newtype MyInt = MkMyInt Int

instance Eq MyInt where (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

--{-# MINIMAL compare | (<=) #-} Ord
--{-# MINIMAL (==) | (/=) #-} Eq

instance Ord MyInt where (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromInteger int)

instance Show MyInt where show (MkMyInt i) = "MkMyInt " ++ show i

instance Fractional MyInt where 
    (/) (MkMyInt i) (MkMyInt j) = MkMyInt (floor ((toRational i) / (toRational j)))
    fromRational i = MkMyInt (floor i)




------------------------------------------------------------------------------
--Zadanie 6
{-#LANGUAGE KindSignatures #-}

class Mappable t where
    fMap :: (a -> b) -> t a -> t b
  
data Vec3D a = Vec3D {xV::a, yV::a, zV::a} deriving Show
  
instance Mappable Vec3D where
    fMap f (Vec3D x y z) = Vec3D (f x) (f y) (f z)

--For tree:
instance Mappable BinTree where
    fMap f EmptyBT = EmptyBT
    fMap f (NodeBT a lt rt ) = NodeBT (f a) (fMap f lt) (fMap f rt)

newtype Pair a = Pair (a,a) deriving Show

instance Mappable Pair where
  fMap f (Pair (x,y)) = Pair (f x, f y)

instance Mappable Maybe where
    fMap f Nothing  = Nothing
    fMap f (Just a) = Just (f a)

instance Mappable (Either a) where
    fMap f (Right a) = Right (f a) 

--https://stackoverflow.com/questions/5195254/understanding-how-either-is-an-instance-of-functor

-- instance Mappable ((->) a) where
--     fMap f g = f `->` g
-- *Main> :k (->) tak samo jak przy Either
--(->) :: * -> * -> *

data Vec2D a = Vec2D a a deriving Show

class VectorLike t where
 (|==|) :: Eq a => t a -> t a -> Bool
 (|+|), (|-|) :: (Num a) => t a -> t a -> t a
 (|*|) :: (Num a) => t a -> t a -> a
 (||?), (|-?) :: (Num a, Eq a) => t a -> t a -> Bool -- równoległość i prostopadłość
 vectLength :: Floating a => t a -> a
 unitVectOf :: Floating a => t a -> t a

instance VectorLike Vec2D where
    (|==|) (Vec2D a b) (Vec2D c d) = (a==c) && (b==d)
    (|+|) (Vec2D a b) (Vec2D c d) = Vec2D (a+c) (b+d)
    (|-|) (Vec2D a b) (Vec2D c d) = Vec2D (a-c) (b-d)
    (||?) (Vec2D a b) (Vec2D c d) = (a*c-b*d) == 0
    (|-?) (Vec2D a b) (Vec2D c d) = (a*b+c*d) == 0
    vectLength (Vec2D a b) = sqrt(a^2+b^2)
    unitVectOf (Vec2D a b) = (Vec2D 1 1)
    (|*|) (Vec2D a b) (Vec2D c d) = a*c + b*d -- ??


-----------------------------------------------------------------------------
--1 Zdefiniuj funktor:
--Funktor to mapowanie pomiedzy kategoriami. W praktyce oznacza to typ po ktorym mozemy mapowac.
--Aby to zrobic musimy zdefiniowac instancje Functor z minimum fmap

--1 Funktor typu Derive 
--Jesli typ a ma funktor to ma tez cala adata

--Funktor aplikatywny
--Jest to Funktor, ktory ma wiecej mozliwosci niz zwykly funktor ale mniej niz monada.
--Funktor, ktory wspiera applikacje w swoim obszarze dzialania. 

--funkcja w konteksie instancji funktora (gwiazda smierci ;) ). 

--Monoid
-- typ z razem ze swoimi binarnymi operacjami. Glownie lacznosc, agregowalnosc 

--2:
--2. Napisz instancję Eq dla typu:
data MyType = C1 Int | C2 Double Bool

instance Eq MyType where
    (==) (C1 a) (C1 b) = a==b
    (==) (C2 a b) (C2 c d) = (a==c) && (b==d)  
    (==) (C1 a) (C2 b c) = ((round b)==a) && c 
    (==) (C2 a b) (C1 c) = ((round a)==c) && b

--3 Utworz 2 instancje:
data BinTree2 a = NodeBT2 (BinTree2 a) (BinTree2 a) | Leaf2 a
-- *Main> let c = NodeBT2 (Leaf2 10) (Leaf2 11)
-- *Main> let c = NodeBT2 (Leaf2 10) (NodeBT2 (Leaf2 11) (Leaf2 20))

-- Napisz depth
depthOfBT2::BinTree2 a -> Int
depthOfBT2 (Leaf2 a) = 1
depthOfBT2 (NodeBT2 lt rt) = 1+max((depthOfBT2 lt) (depthOfBT2 rt))


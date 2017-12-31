import Data.List

f1 :: Num a => a -> a
f1 = \x -> x-2

f2 :: Floating a => a-> a-> a
f2 = \x -> \y -> sqrt(x**2+y**2)

f3 :: Floating a=> a-> a-> a->a
f3 = \x -> \y -> \z -> sqrt(x**2+y**2+z**2)

f4 :: Num a => a->a
f4 = \x -> 2*x

f5 :: Num a=> a -> a
f5 = \x -> x*2

f6 :: Floating a => a -> a
f6 = \x -> x**2

f7 :: Floating a => a -> a
f7 = \x -> 2**x

sqrt' :: Floating a => a-> a
sqrt' = \x -> sqrt(x)

abs' :: (Num a, Ord a) => a -> a 
abs' = \x -> if x < 0 then -x else x

f7' :: Integral a => a -> Bool
f7' = \x -> if mod x 2 == 0 then True else False

f8 :: Floating a => a -> a
f8 = \x -> let y = sqrt x in 2 * y^3 * (y+3)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2+ sumSqr' xs

sumWith :: Num a => (a->a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sumSqr = sumWith (\x -> x^2)
sum'' = sumWith (\x-> x)
sumAbs = sumWith (\x->abs' x)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x*prod' xs 

prodWith :: Num a => (a->a) -> [a] -> a
prodWith _ [] = 1
prodWith f (x:xs) = (f x) * (prodWith f xs)

prod = prodWith (\x->x)
prodSqr = prodWith (\x-> sqrt x)
prodAbs = prodWith (\x-> abs x)

-- univWith :: Num a => (a->a->a) -> [a] -> a
-- univWith x _ [] = if x == (*) then 1 else 0
-- univWith op f (x:xs) = (f x) `op` (univWith op f xs)

sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

--1 zadanie z list comp do przepisania z map/filter/fold
--2 zadanie: przyklad collection pipeline -> co z tego wychodzi

fact :: Integral t => t -> t
fact 0 = 1
fact x = x*(fact (x-1))

expAproxUpTo :: Int -> Double -> Double
expAproxUpTo n = \x -> sum'[(x^k)/fromIntegral(fact k)|k<-[0..n]]

dfr :: (Double -> Double) -> Double -> (Double->Double)
dfr f h = \x -> ((f (x+h)) - (f x))/h

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = \x -> (x1 (x+h) - x2 x)/h
          where x1 = dfr f h 
                x2 = dfr f h 

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

funcListExt :: [ Double -> Double ]
funcListExt = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x, \x -> sqrt(1+x)]

-- A fixity declaration gives the fixity and binding precedence of one or more operators. 
--The integer in a fixity declaration must be in the range 0 to 9. A fixity declaration may appear anywhere that a type signature appears and, like a type signature, 
--declares a property of a particular operator.

-- There are three kinds of fixity, non-, left- and right-associativity (infix, infixl, and infixr, respectively), 
--and ten precedence levels, 0 to 9 inclusive (level 0 binds least tightly, and level 9 binds most tightly).

extractThird :: (a,b,c) -> c
extractThird (_,_,c) = c

sortDesc :: Ord a => [a] -> [a]
sortDesc x = (reverse.sort) x

sortDesc' :: Ord a => [a] -> [a]
sortDesc' x = reverse (sort x)

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt _ _ [] = True
are2FunsEqAt f g (x:xs) | (f x) /= (g x) = False
                        | otherwise = are2FunsEqAt f g xs

onlyEven :: Integral t => [t] -> [t]
onlyEven [] = []
onlyEven (x:xs) | x `mod` 2 == 0 = x : onlyEven xs
                | otherwise      = onlyEven xs

onlyOdd :: Integral t => [t] -> [t]
onlyOdd [] = []
onlyOdd (x:xs) | x `mod` 2 == 1 = x : onlyOdd xs
               | otherwise = onlyOdd xs

onlyUpper :: [Char] -> [Char]
onlyUpper [] = []
onlyUpper (x:xs) | var <= 90 && var >= 65 = x:onlyUpper xs
                 | otherwise = onlyUpper xs
                  where var = fromEnum x::Int

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = \xs -> [x | x<-xs, p x]

onlyEven' :: [Int] -> [Int]
onlyEven' x = filter (\y -> y `mod` 2 == 0) x 

onlyOdd' :: [Int] -> [Int]
onlyOdd' = \x -> filter (\y -> y `mod` 2 == 1) x

onlyUpper' :: [Char] -> [Char]
onlyUpper' = \x -> filter' (\y -> (fromEnum y::Int) >= 65 && (fromEnum y::Int) <=90) x 

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- evalFuncListAt :: a -> [a -> b] -> [b]
-- evalFuncListAt x [] = []
-- evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

-- #TODO
--8.7

evalFuncListAt' :: a -> [a -> b] -> [b]
evalFuncListAt' x = map ($ x) 

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0

prodWith'' g = foldr' (\x acc -> g x*acc) 1

--foldr jest nieznacznie szybszy
--Ale map jest najszybszy

map'' :: (a->b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = (f x): map'' f xs

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc [] = True
isSortedAsc (x:xs) | length (x:xs) == 1 = True
                   | otherwise = head(zipWith (<=) (x:xs) (tail (x:xs))) && isSortedAsc xs
--Znacznie szybsza wersja:                   
isSortedAsc' :: Ord a => [a] -> Bool
isSortedAsc' x = loop True y
                where y = zipWith (<=) x (tail x)
                      loop acc [] = acc
                      loop acc (y:ys) = loop (acc && y) ys 

-- everySecond :: [t] -> [t]
-- everySecond x 

zip' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip' [] _ _ = []
zip' _ [] _ = []
zip' _ _ [] = []
zip' (x:xs) (y:ys) (z:zs) = (x,y,z):zip' xs ys zs 

unzip' :: [(a,b,c)] -> ([a],[b],[c])
unzip' [] = ([],[],[])
unzip' ((x,y,z):xyzs) = (x:xs,y:ys,z:zs)
                        where (xs,ys,zs) = unzip'(xyzs)  




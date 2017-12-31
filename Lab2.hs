printHello = putStrLn "Hello"

add2T :: Num a=> (a,a)->a
add2T (x,y) = x+y

add2C :: Num a=>a->a->a
add2C x y = x+y

add2D :: Num a=>a->a->(a,a)
add2D x y = (x,y)

add2E :: Num a=>a->(a->a)
add2E x y = x+y

add3T :: Num a=>(a,a,a) -> a
add3T (x,y,z) = x+y+z

add3C :: Num a=>a->a->a->a
add3C x y z = x+y+z

--Jak ta konwencja zwrocic inny typ niz wejsciowy?

add2P :: (Num a, Integral b) => b -> a
add2P x = 1 -- ten sam podtyp?

-- add2O :: (Bool a, Num b) => a->b
-- add2O x = True

add2U :: Integral a => a -> a -> a --Dlaczego z integral-em dziala a z Int/Integer juz nie?
add2U x y | x > y = 1
          | otherwise = -1
--Integral vs Integer vs Int ??
--Int jest skonczony, Integer juz nie
--Integral jest rozszerzonym typem Integera?
--https://mail.haskell.org/pipermail/beginners/2011-July/007807.html

--Bo w deklaracji typu na poczatku dajemy klase, na przyklad Integral.
--Dalej jest definicja subklasy, na przyklad Int, Integer

curry2 :: ((a,b)->c) -> a -> b -> c
curry2 f x y = f (x,y)

curry3 :: ((a,b,c)->d) -> a -> b -> c -> d
curry3 f = \x y z -> f(x,y,z)

uncurry2 :: (a->b->c) -> (a,b) -> c
uncurry2 f  (x,y) = f x y

uncurry3 :: (a->b->c->d) -> (a,b,c)->d
uncurry3 f (x,y,z) = f x y z

fiveToPower_ :: Integer -> Integer
fiveToPower_ = \x -> 5^x

_ToPower5 :: Num a => a -> a
_ToPower5 = \x -> x^5

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = \x -> 5-x

subtr5From_ :: Num a => a -> a
subtr5From_ = \x -> x-5

flip' :: (a->b->c) -> b -> a -> c
flip' f = \x y -> f y x

flip3' :: (a->b->c->d) -> c -> b -> a -> d
flip3' f = \x y z -> f z y x


--listy:
-- reverse jedynie zwraca liste z odwroconymi elementami.
--nie zmienia listy oryginalnej
--generalnie operatory na listach nie zmieniaja oryginalnej a zwracaja nowa
-- aby uzyskac inf o dowolnym elemencie wystarczy wspiac xs !! n
--UWAGA: LISTY SA INDEKSOWANE OD 1 A NIE 0
--Podstawowe operacje:
--inicjalizacja: [x| x -> [0..20] , x^2 < 30]
--take n xs
--drop n xs
--head, tail
--null xs -> bool
--zip robi krotki : zip xs ['a','b'] == [(1,a),(2,b)]. Dopoki krotszy zbior sie nie skoczny
--split robi krotke po ntym elemencie, znajduja sie w niej 2 listy

isPalindrome :: [Char] -> Bool
isPalindrome x = x == reverse x

getElAtId :: ([Int],Int) -> Int
getElAtId (x:xs,n) = if n == 0
                     then x
                     else getElAtId (xs,n-1)

capitalize :: [Char] -> [Char]
capitalize (x:xs) = if var <= 90
                    then x:xs
                    else upperVar : xs
                    where var = fromEnum x::Int
                          upperVar = toEnum(var-32)::Char

-- [(i,j) | i <- [0..3], j <- [0..2]]
-- w tym przypadku i jest forem zewnetrznym a j wewnetrznym

--length [(a,b,c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2+b^2==c^2,a+b>c]

isPrime :: Integral t => t -> Bool
isPrime n = [i|i<-[2..n-1],n `mod`i==0]==[]

generatePrime :: Int
generatePrime = length [i|i<-[1..10000],isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

checkList :: ([Int],Int) -> Bool
checkList ((x:xs),n) | x == n = True
                     | x > n = False
                     | otherwise = checkList (xs,n)

isPrime' :: Int -> Bool 
isPrime' n = checkList (primes,n)

generatePrimesTillN :: ([Int],Int,Int) -> Int
generatePrimesTillN ((x:xs),i,n) | x > n = i
                                 | otherwise = generatePrimesTillN (xs,i+1,n)
howManyPrimes :: Int -> Int
howManyPrimes n = generatePrimesTillN (primes,0,n)

fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)
 
fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

-- sum :: Num a => [a] -> a
-- sum []   = 0
-- sum x:xs = x + sum xs

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' (x:xs) | length xs == 0 = x
             | otherwise = x*(prod' xs)

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 1
length' (x:xs) = 1+length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) | x == True = True
           | otherwise = or' xs

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' n [] = False
elem' n (x:xs) | x == n = True
               | otherwise = elem' n xs

doubleAll :: Num t => [t] -> [t] -- double doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = 2*x:doubleAll xs

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs) | x `mod` 2 == 0 = x:selectEven xs
                  | otherwise = selectEven xs
aritmetic :: [Int] -> Double
aritmetic x = z / y
              where y = fromIntegral(length x)::Double
                    z = fromIntegral(sum' x)::Double

isOdd :: (Ord a, Num a) => a -> Bool
isOdd n | n <= 0    = False
        | otherwise = isEven (n-1)

isEven :: (Ord a, Num a) => a -> Bool
isEven n | n < 0     = False
         | n == 0    = True
         | otherwise = isOdd (n-1)

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]
  
qSort' :: Ord a => [a] -> [a]
qSort' []     = []
qSort' (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<x) xs
   rightPart xs = filter (>x) xs

connect :: ([Int],[Int]) -> [Int]
connect (xs,[]) = xs
connect ([],ys) = ys
connect ((x:xs),(y:ys)) | x < y = x:connect (xs,(y:ys))
                        | otherwise = y:connect((x:xs),ys)

mSort :: [Int] -> [Int]
mSort (x:xs) | (length xs) < 2 = connect ([x],xs)
             | otherwise = connect(mSort y,mSort z)
          where lengthVal = (length (x:xs)) `div` 2
                y = take lengthVal (x:xs)
                z = drop lengthVal (x:xs)

insert :: (Int,[Int]) -> [Int] --(insertedElement, resultList)
insert (num,x) | length x == 0 = [num]
               | otherwise = (filter (<num) x) ++ [num] ++ (filter (>num) x)

iSort :: [Int] -> [Int]
iSort (x:xs) = loop ([],(x:xs))
        where loop (resultList,[]) = resultList
              loop (resultList,(x:xs)) = loop(insert(x,resultList),xs)

concat' :: [[a]]->[a]
concat' [] = []
concat' (x:xs)= x++concat'(xs) --elementem jest lista, ciekawe


--Kolos:
-- curry2 :: ((a,b)->c) -> a -> b -> c
-- curry2 f x y = f (x,y)

-- uncurry2 :: (a->b->c) -> (a,b) -> c
-- uncurry2 f  (x,y) = f x y

-- Kartkowka 2:
-- 1. Napisaæ funkcjê f = 2*x przy u¿yciu sekcji
-- 2a. Napisaæ sygnaturê funkcji zipWith
-- 2b. Kwadraty parzystych liczb z przedzia³u [1..10] (list comprehension)
-- 3. Iloczyn elementow listy przy uzyciu rekursji ogonowej (akumulatora)

--1
fFunction :: Num a => a -> a
fFunction = \x -> 2*x

--2a
--(a->b->c)->[a]->[b]->[c]

--2b
square' :: Integral t => [t] -> [t]
square' [] = []
square' (x:xs) | x `mod` 2 == 0 = x^2:square' xs
               | otherwise = square' xs

--3
prod'' :: Num t => [t] -> t
prod'' x = loop 1 x
          where loop acc [] = acc
                loop acc (x:xs) = loop (acc*x) xs

-- 1. Zrobiæ section dodaj¹cy +3
-- 2.a) napisaæ sygnature uncurry
-- b) lista szecianów nieparzystych liczb [1..10] 
-- 3. selectEven :: Integer a => [a] -> [a] rekursja ogonowa

--2a
--(a->b->c)->(a,b)->c

--2b
pow3 :: Integral t => [t] -> [t]
pow3 [] = []
pow3 (x:xs) | x `mod` 2 == 1 = (x^3):pow3 xs
            | otherwise = pow3 xs

selectEven' :: [Int] -> [Int]
selectEven' x = loop [] x
            where loop acc [] = acc
                  loop acc (x:xs) | x `mod` 2 == 0 = loop (acc++[x]) xs
                                  | otherwise = loop acc xs 

-- 1. Co poka¿e :t

-- f1:: Num t=>t->t->(t,t)->t

-- :t f1 1 2 --?


-- 2. Co zwróc¹ poni¿sze list comprehensions:

-- [(3,j) |i<-[2,1],j<-[i..2]]

-- [[i^2,j^2]|i<-[1..2],j<-[1..2],j>i]


-- 3. Uzupe³niæ definicjê i zrobiæ z tego rekurencjê ogonow¹
-- sumAbs :: Num a=>[a]->a

-- sumabs []=__

-- sumAbs __=__ __ sumAbs __

sumAbs' :: Num a => [a]->a
sumAbs' [] = 0
sumAbs' (x:xs) = (abs x) + sumAbs' xs

sumAbs'' :: Num a => [a] -> a
sumAbs'' x = loop 0 x
           where loop acc [] = acc
                 loop acc (x:xs) = loop (acc + abs x) xs

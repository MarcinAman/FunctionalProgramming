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

-- f9 :: Integral a => a -> a
-- f9 = \x -> 
-- f9 = \x -> 0

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
sum = sumWith (\x-> x)
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

   
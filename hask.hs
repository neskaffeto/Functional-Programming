import Distribution.Simple.Setup (trueArg)
foo :: Int
foo = 43

add :: Int -> Int -> Int
add = (+)
---add \x -> \y -> x + y  --lambda funkciq
--add x y = x + y

add1 :: Int -> Int
--add1 x = x+1
add1 = (+1)

right :: baba -> baba -> baba
right _ y = y

tripleEq :: (Eq a) => a -> a -> a -> Bool --tipa a moje da se sravnqva
tripleEq x y z = (x == y) && (y == z)

sumNums :: [Int] -> Int
sumNums [] = 0
--sumNums l = (head l) + (sumNums tail l)
sumNums (h:t)= h + (sumNums t)





---zad1
fact :: Int -> Int
fact n 
    |n == 0   =  1
    |otherwise =  n * fact (n-1)

--zad2 
fib :: Int -> Int
fib n 
    |n == 0 = 0
    |n == 1 = 1
    |otherwise = fib(n-1) + fib(n-2)

--zad3???
myAbs :: Int -> Int
myAbs n 
    |n < 0 = -n
    |otherwise  = n 

--zad4
composeInt :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
composeInt f g x = f (g x)

--zad5
compose :: (a -> a) -> (a -> a) -> (a -> a)
compose f g x = f (g x)

--zad6
myConcat :: [a] -> [a] -> [a]
myConcat [] l2 = l2
myConcat (h1:t1) l2 = h1: myConcat t1 l2

--zad7 
isIntPrefix :: [Int] -> [Int] -> Bool
isIntPrefix [] l2 = True
isIntPrefix l1 [] = False 
isIntPrefix (h1:t1) (h2:t2) =
    if h1 /= h2 then False
    else isIntPrefix t1 t2
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
len :: [a] -> Int
len [] = 0
len(_:xs) = 1 + len xs

-- exists _ [] = False
-- exists p (x:xs) = p x || exists p xs

exists :: (a->Bool) -> [a] -> Bool
exists p = foldr(\x b -> p x || b) False

member :: Eq a => a -> [a] -> Bool
member x = exists (== x)

--5ta
listMap ::  (a->a) -> [a] -> [a]
listMap _ [] = []
listMap f (x:xs)= f x: listMap f xs

--6ta
listFilter :: (a->Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter p (x:xs) = if p x then x: listFilter p xs else listFilter p xs

--7ma
push :: a -> [a] -> [a]
--push x [] = [x];
--push x (h:t) = h: push x t 

--push x l = foldr (:) [x] l
push x = foldr (:) [x] 

--8ma
myrev :: [a] -> [a]
--myrev [] = []
--myrev (x:xs) = push x (myrev xs)
--myrev = foldr push []
myrev = foldl (flip (:)) []

--9ta

insert :: a -> Int -> [a] -> [a]
insert a 0 l = a: l
insert a _ [] = [a]
insert a n (x:xs) = x : insert a (n-1) xs 

--10ta
append :: [a] -> [a] -> [a]
append = flip (foldr (:))

--13ta
mysum :: [Int] -> Int
mysum  = foldr (+) 0 

--14ta
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort(filter (<=x) xs ) ++ [x] ++ quickSort(filter (>x) xs)
--quickSort (x:xs) = x

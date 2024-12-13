{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

import Data.Sequence (Seq(Empty))
import System.Win32 (COORD(xPos))
--algebrichni tipove

data MyBool
    = MyTrue
    | MyFalse
    deriving (Show, Eq)

mynot :: MyBool -> MyBool
mynot MyTrue = MyFalse
mynot MyFalse = MyTrue

data Shape
    = Circle Float
    | Rectangle Float Float
    | Ngon Int Float
    | Point
   -- | UnionShape Shape Shape
    --deriving (Show)
--zad1
area :: Shape -> Float
area Point = 0
area (Circle r) = r * r * pi
area (Rectangle a b) = a * b
--area (UnionShape s1 s2) = area s1 + area s2
area (Ngon _ _) = error "too lazy"
--zad2
perimeter :: Shape -> Float
perimeter Point = 0
perimeter (Rectangle a b) = (a + b) * 2
perimeter (Circle r) = 2 * r * pi
perimeter (Ngon sides a) = fromIntegral sides * a 

instance (Show n) => Show (Shape n) where
    show (Circle f) = "circle with radius " ++ (show f)
    show (Rectangle a b) = "rectangle with sides " ++ (show a) ++ " and " ++ (show b)
    show (Ngon n r) = "ngon with " ++ (show n) ++ " sides and radius " ++ (show r)
--zad3

data RPS
    = Rock
    | Paper
    | Scissors
    --deriving (Show)

instance Show RPS where
    show Rock = "🗿"
    show Paper = "🧻"
    show Scissors = "/\\"

beats :: RPS -> RPS -> Bool
beats Scissors Paper = True
beats Rock Scissors = True
beats Paper Rock = True
beats _ _ = False;

data List a
    = Cons a (List a)
    | Emty
    deriving (Show)

lsum :: (Num n) => List n -> n
lsum Emty = 0
lsum (Cons x xs) = x + lsum xs 

--zad 4
lmap :: (Num n) => (n -> n) -> List n -> List n
lmap _ Emty = Emty
lmap f (Cons x xs) = Cons (f x) (lmap f xs)

--zad 5
lfilter :: (Num n) => (n -> Bool) -> List n -> List n
lfilter _ Emty = Emty
lfilter p (Cons x xs) = if p x  then Cons x (lfilter p xs) else lfilter p xs

--zad 6
lfoldr :: (Num n) => (n -> n->  n) -> n -> List n -> n
        -- op nv l
lfoldr _ nv Emty = nv
lfoldr op nv (Cons x xs) = op nv (lfoldr op x xs)

-- ~~~~~~~~~~~~
class Countable a where
    count :: a -> Int

instance Countable (List a) where
    count Emty = 0
    count (Cons _ l) = count l + 1

    --zad 7

class Averageable a where
    avg :: a -> Float

instance Averageable (List Float) where
    avg Emty = 0
    avg list = lsum list/ fromIntegral (count list)


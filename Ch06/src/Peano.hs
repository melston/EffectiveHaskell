{-# LANGUAGE DefaultSignatures, TypeApplications #-}
module Peano where

-- import Natural
import NaturalClass

data Peano = Z | S Peano

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int 
fromPeano Z = 0
fromPeano (S n) = 1 + fromPeano n

instance Eq Peano where
    (==) Z Z = True
    (==) (S a) (S b) = a == b 
    (==) _ _ = False

instance Show Peano where
    show Z = "Z"
    show (S a) = "(S " <> show a <> ")"

instance Natural Peano where
    add a Z = a
    add a (S b) = add (S a) b
    multiply Z _ = Z
    multiply (S a) b = add b (multiply a b) 
    additiveIdentity = Z 
    multiplicativeIdentity = S Z

{- 
unique :: Natural a -> [a] -> [a] 
unique _ [] = []
unique n (elem:elems) =
    let
        compare a b = not $ (equal n) a b 
        elems' = filter (compare elem) elems
    in elem : unique n elems'

sumOfUniques :: Natural a -> [a] -> a 
sumOfUniques n =
    foldr (add n) (additiveIdentity n) . unique n 


unique :: NaturalClass.Natural a => [a] -> [a] 
unique [] = []
unique (elem:elems) =
    let
        compare a b = not $ (==) a b 
        elems' = filter (compare elem) elems
    in elem : unique elems'

sumOfUniques :: NaturalClass.Natural a => [a] -> a 
sumOfUniques =
    foldr (add) (additiveIdentity) . unique
-}


showIdentities =
    let mul = multiplicativeIdentity :: Peano
        add = additiveIdentity :: Peano 
        msg = "The additive identity is: "
            <> show add
            <> " and the multiplicative identity is: " 
            <> show mul
    in print msg
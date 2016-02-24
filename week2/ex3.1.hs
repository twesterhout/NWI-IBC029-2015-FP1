-- Tom Westerhout s4469925

module Ex3_1 
    ( Dag(..)
    , Nat(..)
    , Getal(..)
    , Functies(..)
    , Void(..)
    ) where

data Dag = Maandag | Dinsdag | Woensdag | Donderdag | Vrijdag | Zaterdag | Zondag deriving(Show, Read, Eq, Ord)
{- usage:
 - Maandag
 - Maandag == Woensdag
 - Vrijdag >= Maandag
 -}


data Nat = Nat Int deriving(Show, Read, Eq, Ord)
{- usage:
 - Nat 5
 - read "Nat 5" :: Nat
 - Nat 5 == Nat -6
 -}


data Getal = Geheel Nat | Decimaal Float deriving(Show, Read, Eq)
{- usage:
 - Geheel $ Nat 6
 - Decimaal 5.4
 - Geheel (Nat 4) == Decimaal 4.0 ---> False
 -}


data Functies = F0 Int | F1 (Int -> Int) | F2 (Int -> Int -> Int)
{- usage:
 - let test_f0 = F0 123
 - let test_f1 = F1 (\x -> x^2)
 - let test_f2 = F2 (gcd)
 -}


data Void = Void deriving(Show, Read)
{- usage:
 - Void
 - read "Void" :: Void
 -}

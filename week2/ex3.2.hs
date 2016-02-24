-- Tom Westerhout s4469925

module Rational'
    ( toQ
    , zero -- ?
    , one -- ?
    , isInt
    , toInt
    , toFloat
    , (/)
    ) where


data Rational' = Q Int Int

{- As an instance of Num typeclass Rational' implements fromInteger function.
 - Thus consider using (0 :: Rational') and (1 :: Rational'), instead of 'zero' and 'one', 
 - That way you not only get 0 and 1, but any Integer literal may be viewed as a Rational'
 -}
zero :: Rational'
zero = Q 0 1

one :: Rational'
one = Q 1 1


{- Smart constructor.
 - Makes sure denominator is > 0
 -}
toQ :: Int -> Int -> Rational'
toQ p q
    | q == 0    = error "Division by 0 not permitted!"
    | q < 0     = toSimple $ Q (-p) (-q)
    | otherwise = toSimple $ Q p q


instance Eq Rational' where
    (==) (Q 0 q) (Q 0 y) = True
    (==) (Q p q) (Q x y) = (p == x) && (q == y)


instance Show Rational' where
    show (Q p q)    
        | isInt (Q p q)     = (show p)
        | otherwise         = (show p) ++ "/" ++ (show q)


instance Ord Rational' where
    (<) (Q p q) (Q x y) = (p * y) < (x * q)
    (<=) (Q p q) (Q x y) = (p * y) <= (x * q)


instance Num Rational' where
    (Q p q) + (Q x y) = toSimple $ Q (p*y + x*q) (q*y)

    negate (Q p q) = Q (-p) q

    (Q p q) * (Q x y)
        | (Q p q) == zero  = zero
        | otherwise         = Q (p * x `div` gcd_py `div` gcd_xq) 
                                (q * y `div` gcd_py `div` gcd_xq)
                                    where   gcd_py = gcd p y
                                            gcd_xq = gcd x q

    abs (Q p q) = Q (abs p) q

    signum (Q p q)
        | p == 0    = zero
        | otherwise = Q (signum p) 1

    fromInteger a   = Q (fromInteger a :: Int) 1


instance Fractional Rational' where
    (Q p q) / (Q x y)     
        | (Q x y) == zero  = error "Division by 0 not permitted"
        | otherwise         = Q (p * y `div` gcd_px `div` gcd_yq) 
                                (q * x `div` gcd_px `div` gcd_yq)
                                    where   gcd_px = gcd p x
                                            gcd_yq = gcd y q
    
    fromRational = error "not implemented"


toSimple :: Rational' -> Rational'
toSimple (Q p q)    = Q (p `div` gcd_pq) (q `div` gcd_pq)
                            where gcd_pq = gcd p q


isInt :: Rational' -> Bool
isInt (Q p q)  = (q == 1)


toInt :: Rational' -> Int
toInt = round . toFloat


toFloat :: Rational' -> Float
toFloat (Q p q) = (fromIntegral p) / (fromIntegral q)



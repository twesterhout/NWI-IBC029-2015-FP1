-- Tom Westerhout s4469925

module Vector2
    ( Vector2(..)
    , zero -- ?
    , one -- ?
    ) where


data Vector2 a = Vector2 { x0 :: a
                         , x1 :: a 
                         } deriving(Eq, Ord)


{- Remarks from ex3.2.hs apply to 'Vector2 a' as well
 -}
zero :: (Integral a) => Vector2 a
zero = Vector2 0 0

one :: (Integral a) => Vector2 a
one = Vector2 1 1


instance (Show a) => Show (Vector2 a) where
    show (Vector2 x0 x1) =  "("
                            ++ show x0
                            ++ ", "
                            ++ show x1
                            ++ ")"


instance (Num a) => Num (Vector2 a) where
    (Vector2 x0 x1) + (Vector2 y0 y1) = Vector2 (x0 + y0) (x1 + y1)
    (Vector2 x0 x1) - (Vector2 y0 y1) = Vector2 (x0 - y0) (x1 - y1)
    (Vector2 x0 x1) * (Vector2 y0 y1) = Vector2 (x0 * y0) (x1 * y1)
    abs = error "not implemented"
    signum = error "not implemented"
    fromInteger n = Vector2 (fromInteger n) (fromInteger n)


instance (Fractional a) => Fractional (Vector2 a) where
    (Vector2 x0 x1) / (Vector2 y0 y1) = Vector2 (x0 / y0) (x1 / y1)
    fromRational = error "not implemented"
    

module Time 
    ( Time
    , toTime
    ) where

data Time = Time { min :: Int, sec :: Int } deriving (Eq, Ord)


toTime :: Int -> Int -> Time
toTime m s 
    | m < 0 || s < 0    = error "Time constructor does not support negative arguments"
    | s >= 60           = Time (m + s `div` 60) (s `mod` 60)
    | otherwise         = Time m s


instance Show Time where
    show (Time m s) = (show m) ++ ":" ++ (show s)


instance Num Time where
    (Time m1 s1) + (Time m2 s2)     =   let s = s1 + s2 
                                        in Time (m1 + m2 + s `div` 60 ) (s `mod` 60)
    (Time m1 s1) - (Time m2 s2)
        | (m1, s1) < (m2, s2)       = Time 0 0
        | otherwise                 = reformat__ $ Time (m1 - m2) (s1 - s2)
                                        where reformat__ (Time m s)
                                                | s < 0     = Time (m - 1) (s + 60)
                                                | otherwise = Time m s

    fromInteger n                   = Time (fromInteger (n `div` 60) :: Int) (fromInteger (n `mod` 60) :: Int)

    (*) _ _ = error "Not implemented"
    signum _ = error "Not implemented"
    abs _ = error "Not implemented"

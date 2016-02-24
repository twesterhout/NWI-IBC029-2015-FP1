module ExSet
    ( Tree
    , add
    , delete
    , union
    , difference
    , intersection

    , toSet
    , fromSet
    , isEmptySet
    , isDisjoint
    , memberOfSet
    , nrOfElements
    , isStrictSubset
    , isSubset
    , without
    , ExSet.product
    , powerSet
    ) where

data Tree t = E__ | T__ t Int (Tree t) (Tree t)
type Set a = Tree a

-- FUNCTIONS FROM THE EXERCISE --

toSet :: (Ord t) => [t] -> (Set t)
toSet = fromList

fromSet :: (Set t) -> [t]
fromSet = toList

isEmptySet :: (Set t) -> Bool
isEmptySet = null

memberOfSet :: (Ord t) => t -> (Set t) -> Bool
memberOfSet = ExSet.elem

nrOfElements :: (Set t) -> Int
nrOfElements = length

without :: (Ord t) => (Set t) -> (Set t) -> (Set t)
without = difference

isStrictSubset :: (Ord t) => (Set t) -> (Set t) -> Bool
isStrictSubset a b = (intersection a b == a) && (length a < length b)

isSubset :: (Ord t) => (Set t) -> (Set t) -> Bool
isSubset a b = intersection a b == a

isDisjoint :: (Ord t) => (Set t) -> (Set t) -> Bool
isDisjoint a b = E__ == intersection a b

product :: (Ord t1, Ord t2) => (Set t1) -> (Set t2) -> (Set (t1, t2))
product s1 s2 = toSet [(x1, x2) | x1 <- fromSet s1, x2 <- fromSet s2] 


powerSet :: (Ord t) => (Set t) -> (Set (Set t))
powerSet = fromList . (map (fromList)) . powerSetList . toList
    where   powerSetList []     = [[]]
            powerSetList (x:xs) = (powerSetList xs) ++ (map (x:) (powerSetList xs))

-- FOR DEBUGGING --
instance (Show t) => Show (Tree t) where
    show = show . toList


-- IMPLEMENTING BALANCED BINARY SEARCH TREES --



-- A constant used for balancing the tree
-- The following should hold at all times:
--      for a tree (T__ _ _ l r):
--          (weight * length l >= length r) && (length l <= weight * length r)
weight :: Int
weight = 4

instance Foldable Tree where
    foldr f base E__            = base
    foldr f base (T__ v _ l r)  = foldr f (f v (foldr f base r)) l

    length E__                = 0
    length (T__ _ count _ _)  = count
    
    maximum (T__ v _ _ E__) = v
    maximum (T__ v _ _ r)   = (maximum r)
    
    minimum (T__ v _ E__ _) = v
    minimum (T__ v _ l _)   = (minimum l)


instance (Ord t) => Eq (Tree t) where
    (==) E__ E__            = True
    (==) E__ (T__ _ _ _ _)  = False
    (==) tree1@(T__ _ n1 _ _) (T__ v2 n2 l2 r2) = 
        let l1' = split_lt tree1 v2
            r1' = split_gt tree1 v2
        in 
            (n1 == n2) && (v2 `ExSet.elem` tree1) && (l1' == l2) && (r1' == r2)


instance (Ord t) => Ord (Tree t) where
    (<) tree1 tree2
        | length tree1 < length tree2   = True
        | length tree1 > length tree2   = False
        | otherwise                     = (<) (toList tree1) (toList tree2)
    (<=) tree1 tree2 
        | length tree1 < length tree2   = True
        | length tree1 > length tree2   = False
        | otherwise                     = (<=) (toList tree1) (toList tree2)


toList :: (Tree t) -> [t]
toList = foldr (:) []

fromList :: (Ord t) => [t] -> (Tree t)
fromList = foldr (\x tree -> add tree x) E__

elem :: (Ord t) => t -> Tree t -> Bool
elem x E__ = False
elem x (T__ v _ l r)
    | x < v     = ExSet.elem x l
    | x > v     = ExSet.elem x r
    | otherwise = True


-- Smart constructor
sN__ :: t -> (Tree t) -> (Tree t) -> (Tree t)
sN__ v l r = T__ v (1 + length l + length r) l r


single_L :: t -> (Tree t) -> (Tree t) -> (Tree t)
single_L a x (T__ b _ y z) = sN__ b (sN__ a x y) z

double_L :: t -> (Tree t) -> (Tree t) -> (Tree t)
double_L a x (T__ c _ (T__ b _ y1 y2) z) = sN__ b (sN__ a x y1) (sN__ c y2 z)

single_R :: t -> (Tree t) -> (Tree t) -> (Tree t)
single_R b (T__ a _ x y) z = sN__ a x (sN__ b y z)

double_R :: t -> (Tree t) -> (Tree t) -> (Tree t)
double_R c (T__ a _ x (T__ b _ y1 y2)) z = sN__ b (sN__ a x y1) (sN__ c y2 z)


-- Smart constructor for the case when the original 
-- tree was in balance and then l or r has changed in size
-- by at most one element
sT'__ :: t -> (Tree t) -> (Tree t) -> (Tree t)
sT'__ v l r =
    let ln = length l
        rn = length r
    in if (ln + rn < 2) 
        then sN__ v l r
        else if (rn > weight * ln) -- case 1: right subtree is too heavy
            then
                let T__ _ _ rl rr = r
                    rln = length rl
                    rrn = length rr
                in if (rln < rrn)  
                    then single_L v l r
                    else double_L v l r
            else if (ln > weight * rn) -- case 2: left subtree is too heavy
                then 
                    let T__ _ _ ll lr = l
                        lln = length ll
                        lrn = length lr
                    in if (lrn < lln)
                        then single_R v l r
                        else double_R v l r
            else
                sN__ v l r -- case 3: subtrees are balanced






add :: (Ord t) => (Tree t) -> t -> (Tree t)
add E__ x       = T__ x 1 E__ E__
add tree@(T__ v _ l r) x
    | x < v     = sT'__ v (add l x) r
    | x > v     = sT'__ v l (add r x)
    | otherwise = tree


delete :: (Ord t) => (Tree t) -> t -> (Tree t)
delete E__ x    = E__
delete (T__ v _ l r) x
    | x < v     = sT'__ v (delete l x) r
    | x > v     = sT'__ v l (delete r x)
    | otherwise = delete' l r
        where   delete' E__ r   = r
                delete' l E__   = l
                delete' l r = sT'__ (minimum r) l (delmin__ r)
                
-- Remove the minimal element from the tree
-- (left most)
delmin__ :: (Tree t) -> (Tree t)
delmin__ (T__ _ _ E__ r) = r
delmin__ (T__ v _ l r) = sT'__ v (delmin__ l) r

-- Concatenate a node v with two trees: l and r
-- Assertion:   v is greater than any element of l
--              v is smaller than any element of r
concat3 :: (Ord t) => t -> (Tree t) -> (Tree t) -> (Tree t)
concat3 v E__ r = add r v
concat3 v l E__ = add l v
concat3 v l@(T__ v1 n1 l1 r1) r@(T__ v2 n2 l2 r2)
    | weight * n1 < n2  = sT'__ v2 (concat3 v l l2) r2
    | weight * n2 < n1  = sT'__ v1 l1 (concat3 v r1 r)
    | otherwise         = sN__ v l r

-- Concatenate two trees: l and r
-- Assertion:   every element of l is smaller
--              than any element of r
concat :: (Ord t) => (Tree t) -> (Tree t) -> (Tree t)
concat tree1 E__    = tree1
concat tree1 tree2  = concat3 (minimum tree2) tree1 (delmin__ tree2)


-- Returns a tree containing all the elements of
-- the original tree that are smaller than x
split_lt :: (Ord t) => (Tree t) -> t -> (Tree t)
split_lt E__ x  = E__
split_lt (T__ v _ l r) x
    | x < v     = split_lt l x
    | x > v     = concat3 v l (split_lt r x)
    | otherwise = l

split_gt :: (Ord t) => (Tree t) -> t -> (Tree t)
split_gt E__ x  = E__
split_gt (T__ v _ l r) x
    | x > v     = split_gt r x
    | x < v     = concat3 v (split_gt l x) r
    | otherwise = r



union :: (Ord t) => (Tree t) -> (Tree t) -> (Tree t)
union E__ tree2 = tree2
union tree1 E__ = tree1
union tree1 (T__ v _ l r) =
    let l' = split_lt tree1 v
        r' = split_gt tree1 v
    in concat3 v (union l' l) (union r' r)



difference :: (Ord t) => (Tree t) -> (Tree t) -> (Tree t)
difference tree1 E__ = tree1
difference E__ _ = E__
difference tree1 (T__ v _ l r) =
    let l' = split_lt tree1 v
        r' = split_gt tree1 v
    in ExSet.concat (difference l' l) (difference r' r)


intersection :: (Ord t) => (Tree t) -> (Tree t) -> (Tree t)
intersection E__ _ = E__
intersection _ E__ = E__
intersection tree1 (T__ v _ l r) =
    let l' = split_lt tree1 v
        r' = split_gt tree1 v
    in if v `ExSet.elem` tree1
        then concat3 v (intersection l' l) (intersection r' r)
        else ExSet.concat (intersection l' l) (intersection r' r)



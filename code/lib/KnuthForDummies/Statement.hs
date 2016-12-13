{-
    Types and functions relating to statements.
-}

module KnuthForDummies.Statement where

-- A statement
newtype Statement = Statement [Bool]
    deriving (Eq)

-- Nice way of printing statements
instance Show Statement
    where show (Statement bs) = map (\x -> if (x==True) then '1' else '0') bs

-- Smart constructor for statements
makeStatement :: Int                -- Number of atoms
              -> [Int]              -- Which atoms are included
              -> Maybe Statement
makeStatement n included
    | n <= 0 = Nothing
    | any (<0) included || any (>= n) included = Nothing
    | otherwise = Just (Statement bits) where
        bits    = [i `elem` included | i <- [0..(n-1)]]

-- Logical 'or' of two statements
join :: Statement -> Statement -> Statement
join (Statement x) (Statement y) = case (length x == length y) of
    True  -> Statement z where z = zipWith (||) x y
    False -> error "Unequal number of bits."

-- Logical 'and' of two statements
meet :: Statement -> Statement -> Statement
meet (Statement x) (Statement y) = case (length x == length y) of
    True -> Statement z where z = zipWith (&&) x y
    False -> error "Unequal number of bits."

-- Does one statement imply another?
implies :: Statement -> Statement -> Bool
(Statement x) `implies` (Statement y) = case (length x == length y) of
    True  -> all (== True) $ map (\(a,b) -> if a then b else True) $ zip x y
    False -> error "Unequal number of bits."

-- Are the two statements mutually exclusive / disjoint?
disjoint :: Statement -> Statement -> Bool
disjoint (Statement x) (Statement y) = case (length x == length y) of
    True  -> let n = length x in
            (meet (Statement x) (Statement y) == Statement (replicate n False))
    False -> error "Unequal number of bits."

-- An alias for disjoint
mutuallyExclusive :: Statement -> Statement -> Bool
mutuallyExclusive = disjoint

-- Same as implies but the other way around
isImpliedBy :: Statement -> Statement -> Bool
x `isImpliedBy` y = y `implies` x


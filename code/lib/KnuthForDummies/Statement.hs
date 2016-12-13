{-
    Types and functions relating to statements.
-}

module KnuthForDummies.Statement where

-- A statement
newtype Statement = Statement [Bool]
    deriving (Eq, Show)

-- Smart constructor for statements
makeStatement :: Int                -- Number of atoms
              -> [Int]              -- Which atoms are included
              -> Maybe Statement
makeStatement n included
    | n <= 0 = Nothing
    | any (<0) included || any (>= n) included = Nothing
    | otherwise = Just (Statement bits) where
        bits     = [i `elem` included | i <- [0..n]]

-- Logical 'or' of two statements
join :: Statement -> Statement -> Statement
join (Statement x) (Statement y) = Statement z where z = zipWith (||) x y

-- Logical 'and' of two statements
meet :: Statement -> Statement -> Statement
meet (Statement x) (Statement y) = Statement z where z = zipWith (&&) x y


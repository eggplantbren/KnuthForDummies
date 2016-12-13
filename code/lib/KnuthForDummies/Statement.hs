{-
    Types and functions relating to statements.
-}
{-# LANGUAGE RecordWildCards #-}

module KnuthForDummies.Statement where

-- A statement
data Statement = Statement { numAtoms :: Int, bits :: [Bool] }

-- Smart constructor for statements
makeStatement :: Int                -- Number of atoms
              -> [Int]              -- Which atoms are included
              -> Maybe Statement
makeStatement n included
    | n <= 0 = Nothing
    | any (<0) included || any (>= n) included = Nothing
    | otherwise = Just Statement {..} where
        numAtoms = n
        bits     = [i `elem` included | i <- [0..n]]


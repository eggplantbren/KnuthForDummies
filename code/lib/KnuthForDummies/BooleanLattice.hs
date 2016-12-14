{-
    Types and functions relating to Boolean lattices.
-}
{-# LANGUAGE RecordWildCards #-}

module KnuthForDummies.BooleanLattice where

-- Imports
import KnuthForDummies.Statement
import Data.List
import Data.Maybe

-- A data type for Boolean lattices
data BooleanLattice = BooleanLattice {
                                       name       :: String,
                                       numAtoms   :: Int,
                                       statements :: [Statement]
                                     } deriving Show

-- Smart constructor
makeBooleanLattice :: String -> Int -> Maybe BooleanLattice
makeBooleanLattice theName n
    | n <= 0    = Nothing
    | otherwise = if (any isNothing ss)
                    then Nothing
                  else (Just BooleanLattice {..}) where
                    ss = [ makeStatement n x | x <- subsequences [0..(n-1)] ]
                    statements = map fromJust ss
                    numAtoms = n
                    name = theName

-- Return the bottom element
bottom :: BooleanLattice -> Statement
bottom BooleanLattice {..} = statements !! 0

-- Disjoint triples of statements (without bottom element)
disjointTriples :: BooleanLattice -> [(Int, Int, Int)]
disjointTriples BooleanLattice {..} =
    [(i, j, k) |
                 i <- [0..m], j <- [(i+1)..m], k <- [(j+1)..m],
                 (disjoint (ss !! i) (ss !! j)) &&
                 (disjoint (ss !! i) (ss !! k)) &&
                 (disjoint (ss !! j) (ss !! k))]
        where
           ss = statements
           m  = length ss - 1

-- Direct product of two lattices
directProduct :: BooleanLattice -> BooleanLattice -> BooleanLattice
directProduct bl1 bl2 = fromJust (makeBooleanLattice newName n) where
    newName = (name bl1) ++ (name bl2)
    n       = (numAtoms bl1) + (numAtoms bl2)


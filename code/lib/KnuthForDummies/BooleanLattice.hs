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
                                       numAtoms   :: Int,
                                       statements :: [Statement]
                                     } deriving Show

-- Smart constructor
makeBooleanLattice :: Int -> Maybe BooleanLattice
makeBooleanLattice n
    | n <= 0    = Nothing
    | otherwise = if (any isNothing ss)
                    then Nothing
                  else (Just BooleanLattice {..}) where
                    ss = [ makeStatement n x | x <- subsequences [0..(n-1)] ]
                    statements = map fromJust ss
                    numAtoms = n

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


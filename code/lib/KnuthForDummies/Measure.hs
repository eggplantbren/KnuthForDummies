{-
    Types and functions relating to measures.
-}
{-# LANGUAGE RecordWildCards #-}

module KnuthForDummies.Measure where

-- Imports
import KnuthForDummies.BooleanLattice
import KnuthForDummies.Statement
import System.Random.MWC
import Control.Monad (replicateM)
import Control.Monad.Primitive (RealWorld)

-- Two kinds of measure...
data Style = Positive | Signed
    deriving (Eq, Show)

-- How I'm representing a Measure
data Measure = Measure Style [Double]
    deriving (Eq, Show)

-- Generate a measure to go along with a Boolean lattice
generateMeasure :: BooleanLattice -> Gen RealWorld -> IO Measure
generateMeasure bl gen = do

    -- Generate some values
    u <- replicateM (numAtoms bl) (uniform gen)

    -- Transform to exponential
    let e = map (\x -> (negate . log) (1.0 - x)) u

    return $ Measure Positive e

-- Returns the measure associated with a certain element of a lattice
measureStatement :: (BooleanLattice, Measure) -> Int -> Double
measureStatement (BooleanLattice {..}, Measure _ m) k = sum values where
    values          = map (\(a, b) -> if b then a else 0.0) $ zip m bools
    Statement bools = statements !! k

---- Demonstrate FIDELITY property
--demonstrateFidelity :: BooleanLattice -> Measure -> IO ()
--demonstrateFidelity bl m = do
--    let 


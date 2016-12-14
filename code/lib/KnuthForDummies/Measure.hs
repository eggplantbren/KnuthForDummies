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

-- Demonstrate FIDELITY property
demonstrateFidelity :: BooleanLattice -> Measure -> IO ()
demonstrateFidelity (BooleanLattice {..}) (Measure style m) = do
    putStrLn "The fidelity property states that if x implies y, then the\
              \ value assigned to statement y must be greater than or equal\
              \ to the value assigned to statement x.\n"

    -- Number of statements in the lattice
    let n = length statements

    -- Value of every statement
    let v = map
                (measureStatement (BooleanLattice {..}, Measure style m))
                [0..(n-1)]

    -- All pairs of statements in the lattice
    let pairs = [ (i, j) | i <- [0..(n-1)], j <- [0..(n-1)] ]

    -- Define an action that can be done on a pair
    let testPair (i, j) = do
          let (x, y)   = (statements !! i, statements !! j)
          let (sx, sy) = (show x, show y)
          let (vx, vy) = (v !! i, v !! j)
          if x `implies` y then
            do
              putStr $ sx ++ " implies " ++ sy ++ ". "
              putStr $ "v(" ++ sx ++ ") = " ++ (show vx) ++ ", "
              putStr $ "v(" ++ sy ++ ") = " ++ (show vy) ++ ". "
              putStr $ "Faithful? " ++ (if vy >= vx then "YES" else "NO")
              putStrLn ""
          else
            do { putStr "" }
          return ()

    -- Test all pairs for fidelity.
    mapM_ testPair pairs

    return ()


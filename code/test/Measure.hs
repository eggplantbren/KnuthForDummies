module Main where

-- Imports
import KnuthForDummies.Measure
import KnuthForDummies.BooleanLattice
import System.Random.MWC
import Control.Monad.Primitive (RealWorld)

testGenerateMeasure :: Gen RealWorld -> IO ()
testGenerateMeasure gen = do
    putStrLn "Testing generateMeasure:\n"
    let bl = case (makeBooleanLattice "a" 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    m <- generateMeasure bl gen
    print m
    putStrLn ""

    return ()

testMeasureStatement :: Gen RealWorld -> IO ()
testMeasureStatement gen = do
    putStrLn "Testing measureStatement:\n"
    let bl = case (makeBooleanLattice "a" 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    m <- generateMeasure bl gen
    print bl
    print m
    print $ map (measureStatement (bl, m)) [0..7]
    putStrLn ""

    return ()

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    putStrLn "Tests of Measure functions...\n"

    testGenerateMeasure gen
    testMeasureStatement gen

    return ()


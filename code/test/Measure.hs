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
    let bl = case (makeBooleanLattice "a" 2) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    m <- generateMeasure bl gen
    print bl
    print m
    print $ map (measureStatement (bl, m)) [0..3]
    putStrLn ""

    return ()

testDemonstrateFidelity :: Gen RealWorld -> IO ()
testDemonstrateFidelity gen = do
    putStrLn "Testing demonstrateFidelity:\n"
    let bl = case (makeBooleanLattice "a" 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    m <- generateMeasure bl gen
    demonstrateFidelity bl m

    return ()

main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    putStrLn "Tests of Measure functions...\n"

    testGenerateMeasure gen
    testMeasureStatement gen
    testDemonstrateFidelity gen

    return ()


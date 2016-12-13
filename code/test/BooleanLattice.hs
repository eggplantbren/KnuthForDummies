module Main where

-- Imports
import KnuthForDummies.BooleanLattice

testMakeBooleanLattice :: IO ()
testMakeBooleanLattice = do
    putStrLn "Testing makeBooleanLattice:\n"

    let bl = case (makeBooleanLattice 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    putStrLn $ (show bl) ++ "\n"
    return ()

testBottom :: IO ()
testBottom = do
    putStrLn "Testing bottom:\n"

    let bl = case (makeBooleanLattice 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    putStrLn $ show (bottom bl) ++ "\n"
    return ()

testDisjointTriples :: IO ()
testDisjointTriples = do
    putStrLn "Testing disjoint triples:\n"

    let bl = case (makeBooleanLattice 3) of
                Nothing  -> error "Error."
                Just bl' -> bl'

    putStrLn $ show (disjointTriples bl) ++ "\n"
    return ()

main :: IO ()
main = do
    putStrLn "Tests of BooleanLattice functions...\n"

    testMakeBooleanLattice
    testBottom
    testDisjointTriples

    return ()


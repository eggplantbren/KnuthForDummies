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

main :: IO ()
main = do
    putStrLn "Tests of BooleanLattice functions...\n"

    testMakeBooleanLattice

    return ()


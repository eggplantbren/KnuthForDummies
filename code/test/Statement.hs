module Main where

-- Imports
import KnuthForDummies.Statement
import System.Random.MWC
import Control.Monad.Primitive (RealWorld)
import Control.Monad (replicateM)

-- Test the smart constructor
-- Smart constructor for statements
testMakeStatement :: IO ()
testMakeStatement = do
    let on = [2, 4]
    let x = case (makeStatement 5 on) of
                Nothing -> error "Error."
                Just x' -> x'

    putStrLn "Testing makeStatement:\n"
    putStrLn $ (show x) ++ "\n"

    return ()

-- Test the join
testJoin :: Gen RealWorld -> IO ()
testJoin gen = do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z'  = join x' y'

    putStrLn "Testing join:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x v y = " ++ (show z') ++ "\n"

    return ()

-- Test the meet
testMeet :: Gen RealWorld -> IO ()
testMeet gen = do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z'  = meet x' y'

    putStrLn "Testing meet:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x ^ y = " ++ (show z') ++ "\n"

    return ()

-- Test implies
testImplies :: Gen RealWorld -> IO ()
testImplies gen = do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z' = x' `implies` y'

    putStrLn "Testing implies:\n"
    putStrLn $ "x             = " ++ (show x')
    putStrLn $ "y             = " ++ (show y')
    putStrLn $ "x `implies` y = " ++ (show z') ++ "\n"

    return ()

-- Test disjoint
testDisjoint :: Gen RealWorld -> IO ()
testDisjoint gen = do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z' = disjoint x' y'

    putStrLn "Testing disjoint:\n"
    putStrLn $ "x       = " ++ (show x')
    putStrLn $ "y       = " ++ (show y')
    putStrLn $ "Disjoint? " ++ (show z') ++ "\n"

    return ()


main :: IO ()
main = withSystemRandom . asGenIO $ \gen -> do
    putStrLn "Tests of Statement functions...\n"

    testMakeStatement
    testJoin gen
    testMeet gen
    testImplies gen
    testDisjoint gen

    return ()


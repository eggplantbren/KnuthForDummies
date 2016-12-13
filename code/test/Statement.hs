module Main where

-- Imports
import KnuthForDummies.Statement
import System.Random.MWC
import Control.Monad (replicateM)


-- Test the join
testJoin :: IO ()
testJoin = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z'  = join x' y'

    putStrLn "Testing join:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x v y = " ++ (show z')    

    return ()

-- Test the meet
testMeet :: IO ()
testMeet = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z'  = meet x' y'

    putStrLn "Testing meet:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x ^ y = " ++ (show z')

    return ()

-- Test implies
testImplies :: IO ()
testImplies = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 5 (uniform gen) :: IO [Bool]
    y <- replicateM 5 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let z' = x' `implies` y'

    putStrLn "Testing implies:\n"
    putStrLn $ "x             = " ++ (show x')
    putStrLn $ "y             = " ++ (show y')
    putStrLn $ "x `implies` y = " ++ (show z')

    return ()


main :: IO ()
main = do
    putStrLn "Tests of Statement functions...\n"
    testJoin
    putStrLn ""
    testMeet
    putStrLn ""
    testImplies

    return ()


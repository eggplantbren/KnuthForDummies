module Main where

-- Imports
import KnuthForDummies.Statement
import System.Random.MWC
import Control.Monad (replicateM)


-- Test the join
testJoin :: IO ()
testJoin = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 10 (uniform gen) :: IO [Bool]
    y <- replicateM 10 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let m'  = join x' y'

    putStrLn "Testing join:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x v y = " ++ (show m')    

    return ()

-- Test the meet
testMeet :: IO ()
testMeet = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 10 (uniform gen) :: IO [Bool]
    y <- replicateM 10 (uniform gen) :: IO [Bool]

    let x' = Statement x
    let y' = Statement y
    let m'  = meet x' y'

    putStrLn "Testing meet:\n"
    putStrLn $ "x     = " ++ (show x')
    putStrLn $ "y     = " ++ (show y')
    putStrLn $ "x ^ y = " ++ (show m')    

    return ()

main :: IO ()
main = do
    putStrLn "Tests of Statement functions...\n"
    testJoin
    putStrLn ""
    testMeet

    return ()


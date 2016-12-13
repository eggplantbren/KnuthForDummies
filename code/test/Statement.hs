module Main where

-- Imports
import KnuthForDummies.Statement
import System.Random.MWC
import Control.Monad (replicateM)

-- Test the meet
testMeet :: IO ()
testMeet = withSystemRandom . asGenIO $ \gen -> do
    x <- replicateM 10 (uniform gen) :: IO [Bool]
    let x' = Statement x
    print x'

    return ()

main :: IO ()
main = do
    putStrLn "Tests of Statement functions..."

    testMeet

    return ()


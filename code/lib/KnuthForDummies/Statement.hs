{-
    Types and functions relating to statements.
-}

module KnuthForDummies.Statement where

-- Imports
import Data.Vector.Unboxed as U

-- A statement
data Statement = Statement { numAtoms :: Int, bits :: U.Vector Bool }


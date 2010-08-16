module Confluence.Types (
    Problem (..),
    Proof (..)
) where

import Rewriting.Types
import Types

data Problem = Problem (TRS Fun Var)

data Proof
    = Empty
    | KnuthBendix [(Rewrite Fun Var, Rewrite Fun Var)]
    -- confluence proofs for all CPs
    | NormalForm  (Rewrite Fun Var) (Rewrite Fun Var)
    -- two divergent reductions

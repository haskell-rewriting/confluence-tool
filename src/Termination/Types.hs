module Termination.Types (
    Problem (..),
    Proof (..)
) where

import Rewriting.Types
import Types

data Problem = Problem (TRS Fun Var) (TRS Fun Var) -- relative termination

type XML = String

data Proof
    = Empty
    | TTT2 XML

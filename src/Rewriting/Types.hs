module Rewriting.Types (
    Term (..),
    Rule (..),
    TRS (..),
    Rewrite (..)
) where

data Term f v
    = Var v
    | App f [Term f v]

data Rule f v = Term f v :-> Term f v

newtype TRS f v = TRS [Rule f v]

type Rewrite f v = [Term f v]

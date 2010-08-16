module Task (
    Task (..)
) where

data Task p
    = Prove p
    | Disprove p

--    | Decide p

-- for simplicity omit this for the time being.
-- see Problem.hs (Processor p) for discussion on this.

instance Functor Task where
    fmap f (Prove p)    = Prove  (f p)
    fmap f (Disprove p) = Disprove (f p)
--    fmap f (Decide p)   = Decide (f p)

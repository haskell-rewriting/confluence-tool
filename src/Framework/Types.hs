module Framework.Types (
    Answer (..),
) where

data Answer = Yes | No | Maybe
    deriving (Eq, Ord, Show)

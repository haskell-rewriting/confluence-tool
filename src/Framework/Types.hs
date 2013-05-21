-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Framework.Types (
    Answer (..),
) where

data Answer = Yes | No | Maybe
    deriving (Eq, Ord, Show)

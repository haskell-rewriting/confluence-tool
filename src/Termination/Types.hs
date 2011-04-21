module Termination.Types (
    Problem,
) where

import Data.Rewriting.Rule

type Problem = [Rule String String]

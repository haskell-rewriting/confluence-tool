module Confluence.Types (
    Problem,
) where

import Data.Rewriting.Rule (Rule)

type Problem f v = [Rule f v]

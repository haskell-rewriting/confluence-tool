module Confluence.Type (
    Problem,
) where

import Data.Termlib.TRS

type Problem f v = TRS f v

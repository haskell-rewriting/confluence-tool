-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Confluence.Types (
    Problem,
) where

import Data.Rewriting.Rule (Rule)

type Problem f v = [Rule f v]

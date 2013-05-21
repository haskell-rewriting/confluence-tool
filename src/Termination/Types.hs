-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Termination.Types (
    Problem,
) where

import Data.Rewriting.Rule

type Problem = [Rule String String]

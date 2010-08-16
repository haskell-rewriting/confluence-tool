module Problem (
    Proof (..),
    Problem (..)
) where

import qualified Confluence.Types as C
import qualified Termination.Types as T
import Task

data Problem
    = ConfluenceProblem C.Problem
    | TerminationProblem T.Problem

data Proof
    = ConfluenceProof C.Proof
    | TerminationProof T.Proof

data Processor p = Processor {
    name        :: String,
    description :: String,
    run         :: Task p -> IO (Any (Proof, All (Task Problem)))
}

-- Problems arising from adding a "Decide" task type:
-- - each "Decide" Subproblem gives rise to two possible
--   continuations - how do we encode that? A tree maybe?
--
-- - we need to encode the answer somehow, too.
--
-- - We get different sorts of completeness here
--   version 1) each reduced problem is equivalent to the original problem
--   version 2) only by disproving all the generated subproblems can we
--              disprove the original problem.
--
--   It boils down to this: Is there anything like a CNF for three-valued
--   logic (Yes, Unknown, No)?

type Any a = [a]
type All a = [a]

data ProofTree
    = ProofTree Problem Proof [ProofTree]

data Attempt
    = All     Problem [Attempt]
    | Any     Problem [Attempt]
    | Skipped Problem

data Answer
    = Proved ProofTree
    | Disproved ProofTree
    | Maybe Attempt

{-

data Answer = Yes Proof | Maybe | No Proof

data Log
    = Run Processor (AnyLog (ProofStep, [(Problem, Log)]))
    | Skip

type XML = String

type Any a = [a]

type All a = [a]

data AnyLog a
    = Cons a (AnyLog a)
    | Nil
    | More

------------------------------------------------------------------------
-- how do we deal with the difference between completeness and soundness?
-- one idea is this:

data Task = Decide Problem | Prove Problem | Disprove Problem

-- complete and sound procedures could produce Decision problems from
-- Decision problems, while sound procedures would produce proof problems.

-----------------------------------------------------------------------
-- should we distinguish the various proof types by problem type?
-- (yes)

-}

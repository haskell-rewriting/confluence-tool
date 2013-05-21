-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

-- Knuth-Bendix criterion
{-# LANGUAGE OverloadedStrings #-}
module Confluence.Direct.KnuthBendix (
    confluent
) where

import Text.PrettyPrint.ANSI.Leijen hiding (group)
import qualified Data.Rewriting.Rules as R
import Data.Rewriting.Rule hiding (vars)
import Data.Rewriting.Term (Term (..), vars)
import Data.Rewriting.CriticalPair as C
import Data.List

import Confluence.Types
import Termination.TTT2
import Framework.Types
import Framework.Explain
import Util.Pretty

confluent :: Problem String String -> IO (Explain Answer)
confluent trs = do
    terminating <- ttt2 trs
    return $ section "Knuth-Bendix Criterion" $ case terminating of
        No -> no "not terminating"
        Maybe -> no "possibly not terminating"
        Yes -> do
            tell "The TRS is terminating according to ttt2."
            checkCPs trs (cps' trs)

checkCPs trs [] = do
    tell "No critical pairs left."
    return Yes
checkCPs trs (c:cs) = do
    tell $ "Considering the critical pair"
    tell $ ppretty (C.top c)
    tell $ ppretty (C.left c)
    tell $ ppretty (C.right c)
    let l = nf trs (C.left c)
        r = nf trs (C.right c)
        cont _ = if l == r then do
            tell "Normal forms are equal."
            checkCPs trs cs
          else do
            tell $ "Normal forms are different." <+> hsep (map ppretty (map head . group . sort $ vars (C.top c)))
            checkCPs trs cs
            return No
    maybe (tell "Could not find normal form." >> return Maybe) cont (l >> r)

nf :: (Eq f, Eq v, Ord v') => [Rule f v'] -> Term f v -> Maybe (Term f v)
nf trs = go (10^6) where
    go 0 t = Nothing
    go n t = case map R.result $ R.fullRewrite trs t of
        [] -> Just t
        (t' : ts) -> go (n-1) t'

no :: String -> Explain Answer
no t = section "Knuth-Bendix Criterion" $ do
    tell $ text $ unwords ["The TRS is", t, "according to ttt2."]
    return Maybe

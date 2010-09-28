-- Knuth-Bendix criterion
{-# LANGUAGE OverloadedStrings #-}
module Confluence.Direct.KnuthBendix (
    confluent
) where

import Text.PrettyPrint.HughesPJ
import Data.Termlib.TRS
import Data.Termlib.Term hiding (pretty)
import Data.Termlib.Critical

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
            checkCPs trs (allCPs trs)

checkCPs trs [] = do
    tell "No critical pairs left."
    return Yes
checkCPs trs (c:cs) = do
    tell $ "Considering the critical pair"
    tell $ pretty (cpTop c)
    tell $ pretty (cpLeft c)
    tell $ pretty (cpRight c)
    let l = nf trs (cpLeft c)
        r = nf trs (cpRight c)
        cont _ = if l == r then do
            tell "Normal forms are equal."
            checkCPs trs cs
          else do
            tell "Normal forms are different."
            return No
    maybe (tell "Could not find normal form." >> return Maybe) cont (l >> r)

nf :: (Eq f, Eq v, Ord v') => TRS f v' -> Term f v -> Maybe (Term f v)
nf trs = go (10^6) where
    go 0 t = Nothing
    go n t = case rewrites trs t of
        [] -> Just t
        (t' : ts) -> go (n-1) t'

no :: String -> Explain Answer
no t = section "Knuth-Bendix Criterion" $ do
    tell $ text $ unwords ["The TRS is", t, "according to ttt2."]
    return Maybe

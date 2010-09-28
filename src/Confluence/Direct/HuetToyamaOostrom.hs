{-# LANGUAGE OverloadedStrings #-}
module Confluence.Direct.HuetToyamaOostrom (
    confluent
) where

import Text.PrettyPrint.HughesPJ

import Data.Termlib.Critical
import Data.Termlib.Development
import Data.Termlib.TRS.Props
import qualified Data.Termlib.TermTree as T

import Confluence.Types
import Framework.Types
import Framework.Explain
import Util.Pretty

-- confluent :: (Pretty f, Pretty v, Ord f, Ord v) => Problem f v -> Explain Answer
confluent trs = section "Huet-Toyama-van-Oostrom (development closed)" $ do
    if not (leftlinear trs) then do
        tell "Not left-linear."
        return Maybe
     else do
        let i = innerCPs trs
            o = outerCPs trs
        a <- processInner trs i
        if a /= Yes then return a else do
            processOuter trs o

processInner trs [] = return Yes
processInner trs (c:cs) = do
    tell $ "Considering the inner critical pair" <+> pretty (cpLeft c) <+> "><" <+> pretty (cpRight c)
    let l = development trs (cpLeft c)
        r = [T.fromTerm (cpRight c)]
    if null (T.toList $ T.simplify $ T.intersect l r) then do
        tell "not joinable."
        return Maybe
     else do
        tell "l"
        tell $ nest 4 $ vcat $ map pretty (T.toList l)
        tell "r"
        tell $ nest 4 $ vcat $ map pretty (T.toList r)
        tell "l & r"
        tell $ nest 4 $ vcat $ map pretty (T.toList $ T.intersect l r)
        tell "joinable in a development step."
        processInner trs cs

processOuter trs [] = return Yes
processOuter trs (c:cs) = do
    tell $ "Considering the outer critical pair" <+> pretty (cpLeft c) <+> "><" <+> pretty (cpRight c)
    let l = development trs (cpLeft c)
        r = development trs (cpRight c)
    if null (T.toList $ T.simplify $ T.intersect l r) then do
        tell "not joinable."
        return Maybe
     else do
        tell "joinable using development steps."
        processInner trs cs


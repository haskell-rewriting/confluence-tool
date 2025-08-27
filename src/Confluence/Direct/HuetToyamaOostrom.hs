-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Confluence.Direct.HuetToyamaOostrom (
    confluent
) where

import Text.PrettyPrint.ANSI.Leijen

import Data.Rewriting.CriticalPair as C
import Data.Rewriting.Rules hiding (map)

import Confluence.Types
import Framework.Types
import Framework.Explain
import Util.Pretty

import Development as D

confluent :: (Show f, Show v, PPretty f, PPretty v, Ord f, Ord v) => Problem f v -> Explain Answer
confluent trs = section "Huet-Toyama-van-Oostrom (development closed)" $ do
    if not (isLeftLinear trs) then do
        tell "Not left-linear."
        return Maybe
     else do
        let i = cpsIn' trs
            o = cpsOut' trs
        a <- processInner trs i
        if a /= Yes then return a else do
            processOuter trs o

processInner trs [] = return Yes
processInner trs (c:cs) = do
    tell $ "Considering the inner critical pair"
    tell $ nest 4 $ vcat [
              ppretty (C.top c),
              " =>" <+> ppretty (C.left c),
              " ><" <+> ppretty (C.right c)]
    let l = development trs (C.left c)
        r = [D.fromTerm (C.right c)]
    if null (D.toList $ D.simplify $ D.intersect l r) then do
        tell "not joinable in a development step."
        return Maybe
     else do
        -- tell "l"
        -- tell $ nest 4 $ vcat $ map ppretty (D.toList l)
        -- tell "r"
        -- tell $ nest 4 $ vcat $ map ppretty (D.toList r)
        tell "l & r"
        tell $ nest 4 $ vcat $ map ppretty (D.toList $ D.intersect l r)
        tell "joinable in a development step."
        processInner trs cs

processOuter trs [] = return Yes
processOuter trs (c:cs) = do
    tell $ "Considering the outer critical pair"
    tell $ nest 4 $ vcat [
              ppretty (C.top c),
              " =>" <+> ppretty (C.left c),
              " ><" <+> ppretty (C.right c)]
    let l = development trs (C.left c)
        r = development trs (C.right c)
    if null (D.toList $ D.simplify $ D.intersect l r) then do
        tell "not joinable in a pair of development steps."
        return Maybe
     else do
        tell "l & r"
        tell $ nest 4 $ vcat $ map ppretty (D.toList $ D.intersect l r)
        tell "joinable using development steps."
        processOuter trs cs


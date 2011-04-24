{-# LANGUAGE OverloadedStrings #-}
module Confluence.Direct.Ground (
    confluent
) where

import Confluence.Types
import Framework.Types
import Framework.Explain
import Util.Pretty

import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Rule as Rule
import qualified Data.Rewriting.Term as Term

import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.RWS
import Control.Monad

confluent :: Problem String String -> Explain Answer
confluent trs = section "Confluence for Ground TRSs" $
    if Rules.isGround trs then confluent' trs else do
        tell "Not a ground TRS."
        return Maybe

confluent' :: (Ord f, PPretty f) => Problem f v -> Explain Answer
confluent' trs = do
    let trs' = map (mapRule Term.withArity) trs
        trs2 = tseitin trs'
    section "Tseitin transformation" $
        tell $ vList trs2
    tell "Not yet implemented."
    return Maybe

mapRule :: (Term.Term f v -> Term.Term f' v') -> Rule.Rule f v -> Rule.Rule f' v'
mapRule f (Rule.Rule l r) = Rule.Rule (f l) (f r)

data Fun f  = F f Int | C Int deriving (Eq, Ord, Show)
data Side f = Fun (Fun f) | App (Fun f) (Fun f) deriving (Eq, Ord, Show)

type M f = RWS () [(Side f, Side f)] Int

-- Currying and Tseitin transform.
tseitin :: Problem (f, Int) v -> [(Side f, Side f)]
tseitin trs = snd (evalRWS (mapM go trs) () 0) where
    term :: Term.Term (f, Int) v -> M f (Fun f)
    term = Term.fold var fun
    var _ = error "Not a ground TRS"
    fun (f, a) ts = do
        ts' <- sequence ts
        foldM app (F f a) ts'
    app a b = do
        n <- get
        put $! n+1
        tell [(Fun (C n), App a b), (App a b, Fun (C n))]
        return $ C n
    go :: Rule.Rule (f, Int) v -> M f ()
    go rule = do
        l <- term (Rule.lhs rule)
        r <- term (Rule.rhs rule)
        tell [(Fun l, Fun r)]

instance PPretty f => PPretty (Fun f) where
    ppretty (F f a) = ppretty f <> "/" <> text (show a)
    ppretty (C i) = "c_" <> text (show i)

instance PPretty f => PPretty (Side f) where
    ppretty (Fun f) = ppretty f
    ppretty (App f g) = "(" <> ppretty f <+> "@" <+> ppretty g <> ")"

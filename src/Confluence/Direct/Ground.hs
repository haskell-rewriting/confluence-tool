{-# LANGUAGE OverloadedStrings #-}
module Confluence.Direct.Ground (
    confluent
) where

import Confluence.Types
import Framework.Types
import Framework.Explain
import Util.Pretty
import qualified Util.Relation as R

import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Rule as Rule
import qualified Data.Rewriting.Term as Term

import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.RWS
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M

confluent :: Problem String String -> Explain Answer
confluent trs = section "Confluence for Ground TRSs" $
    if Rules.isGround trs then confluent' trs else do
        tell "Not a ground TRS."
        return Maybe

confluent' :: (Ord f, PPretty f) => Problem f v -> Explain Answer
confluent' trs = do
    let trs' = map (mapRule Term.withArity) trs
        trs2 = tseitin trs'
        trs3 = ffc trs2
        joins = joinc trs3
    section "Tseitin transformation" $
        tell $ vList trs2
    section "Forward closure" $
        tell $ vList $ R.toList trs3
    section "Joinability closure" $
        tell $ vList $ R.toList joins
    tell "Not yet implemented."
    return Maybe

mapRule :: (Term.Term f v -> Term.Term f' v') -> Rule.Rule f v -> Rule.Rule f' v'
mapRule f (Rule.Rule l r) = Rule.Rule (f l) (f r)

data Con f  = F f Int | C Int deriving (Eq, Ord, Show)
data Side f = Con (Con f) | App (Con f) (Con f) deriving (Eq, Ord, Show)

isCon :: Side f -> Bool
isCon Con{} = True
isCon App{} = False

type M f = RWS () [(Side f, Side f)] Int

-- Currying and Tseitin transform.
tseitin :: Problem (f, Int) v -> [(Side f, Side f)]
tseitin trs = snd (evalRWS (mapM go trs) () 0) where
    term :: Term.Term (f, Int) v -> M f (Con f)
    term = Term.fold var fun
    var _ = error "Not a ground TRS"
    fun (f, a) ts = do
        ts' <- sequence ts
        foldM app (F f a) ts'
    app a b = do
        n <- get
        put $! n+1
        tell [(Con (C n), App a b), (App a b, Con (C n))]
        return $ C n
    go :: Rule.Rule (f, Int) v -> M f ()
    go rule = do
        l <- term (Rule.lhs rule)
        r <- term (Rule.rhs rule)
        tell [(Con l, Con r)]

-- -->* closure.
ffc :: Ord f => [(Side f, Side f)] -> R.Rel (Side f)
ffc trs =
    let sides = S.fromList [x | (l, r) <- trs, x <- [l, r]]
        add rel (l, r) | (l, r) `R.member` rel = rel
        add rel (l, r) =
            let rel' = R.insert (l, r) rel
                todo = S.map (\x -> (l, x)) (R.succ rel r) `S.union`
                       S.map (\x -> (x, r)) (R.pred rel l) `S.union`
                       S.fromList congr
                Con fl = l
                Con fr = r
                congr = [(l', r') |
                     isCon l && isCon r,
                     (l', r') <- left ++ right,
                     l' `S.member` sides && r' `S.member` sides]
                left = [(App fl gl, App fr gr) |
                    (Con gl, Con gr) <- R.toList $ R.restrict isCon rel']
                right = [(App gl fl, App gr fr) |
                    (Con gl, Con gr) <- R.toList $ R.restrict isCon rel']
            in  foldl add rel' (S.toList todo)
    in  foldl add (R.fromList [(a, a) | a <- S.toList sides]) trs

joinc :: Ord f => R.Rel (Side f) -> R.Rel (Side f)
joinc trs =
    let sides = S.fromList [x | (l, r) <- R.toList trs, x <- [l, r]]
        add rel (l, r) | (l, r) `R.member` rel = rel
        add rel (l, r) =
            let rel' = R.insert (l, r) rel
                todo = S.map (\x -> (l, x)) (R.pred trs r) `S.union`
                       S.map (\x -> (x, r)) (R.succ trs l) `S.union`
                       S.fromList congr
                Con fl = l
                Con fr = r
                congr = [(l', r') |
                     isCon l && isCon r,
                     (l', r') <- left ++ right,
                     l' `S.member` sides && r' `S.member` sides]
                left = [(App fl gl, App fr gr) |
                    (Con gl, Con gr) <- R.toList $ R.restrict isCon rel']
                right = [(App gl fl, App gr fr) |
                    (Con gl, Con gr) <- R.toList $ R.restrict isCon rel']
            in  foldl add rel' (S.toList todo)
    in  foldl add R.empty [(a, a) | a <- S.toList sides]

instance PPretty f => PPretty (Con f) where
    ppretty (F f a) = ppretty f <> "/" <> text (show a)
    ppretty (C i) = "c_" <> text (show i)

instance PPretty f => PPretty (Side f) where
    ppretty (Con f) = ppretty f
    ppretty (App f g) = "(" <> ppretty f <+> "@" <+> ppretty g <> ")"

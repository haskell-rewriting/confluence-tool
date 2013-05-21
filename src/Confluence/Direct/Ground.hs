-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

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
import Data.List

import Debug.Trace

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
        -- trs3 is the forward closure of the curried TRS, restricted to
        -- the sides of that curried TRS
        joins = joinc trs3
        sides = S.fromList [x | (l, r) <- R.toList trs3, x <- [l, r]]
        stabilizables = stabilizable trs3
{-
    section "Tseitin transformation" $
        tell $ vList trs2
    section "Forward closure" $
        tell $ vList $ R.toList trs3
    section "Joinability closure" $
        tell $ vList $ R.toList joins
-}
    case filter (not . cconds trs3 joins stabilizables) (S.toList sides) of
        s:_ -> do
            tell $ "C-Conditions not satisfied for" <+> ppretty s <> "."
            return No
        [] -> do
            tell "C-Conditions satisfied."
            let ndjs = ndj trs3 joins stabilizables
            case filter (\(s, _) -> (s, s) `R.member` ndjs) (R.toList trs3) of
                (s, _):_ -> do
                    tell $ "Side not deeply joinable with itself:" <+> ppretty s
                    return No
                [] -> do
                    tell $ "All sides deeply joinable with themselves."
                    return Yes

mapRule :: (Term.Term f v -> Term.Term f' v') ->
    Rule.Rule f v -> Rule.Rule f' v'
mapRule f (Rule.Rule l r) = Rule.Rule (f l) (f r)

data Con f  = F f Int | C Int deriving (Eq, Ord, Show)
data Side f = Con (Con f) | App (Con f) (Con f) deriving (Eq, Ord, Show)
data Tops f = Con' (Con f) | App' (Side f) (Side f) deriving (Eq, Ord, Show)

isCon :: Side f -> Bool
isCon Con{} = True
isCon App{} = False

isApp :: Side f -> Bool
isApp Con{} = False
isApp App{} = True

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
    let sides = S.fromList $ trs >>= \(l, r) -> subterms l ++ subterms r
        subterms (Con c) = [Con c]
        subterms (App l r) = [App l r, Con l, Con r]
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

-- joinability closure
joinc :: (Ord f, PPretty f) => R.Rel (Side f) -> R.Rel (Side f)
joinc trs =
    let sides = S.fromList [x | (l, r) <- R.toList trs, x <- [l, r]]
        add rel (l, r) | (l, r) `R.member` rel = rel
        add rel (l, r) =
            let rel' = R.insert (l, r) rel
                todo = S.map (\x -> (l, x)) (R.pred trs r) `S.union`
                       S.map (\x -> (x, r)) (R.pred trs l) `S.union`
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
            in  -- (\res -> traceShow (ppretty (l, r) <$> vList (S.toList todo)) res) $
                foldl add rel' (S.toList todo)
    in  -- (\res -> traceShow ("joinc" <$> vList (S.toList sides)) res) $
        foldl add R.empty [(a, a) | a <- S.toList sides]

-- (top-)stabilizable terms
stabilizable :: Ord f => R.Rel (Side f) -> S.Set (Side f)
stabilizable trs =
    let sides = S.fromList [x | (l, r) <- R.toList trs, x <- [l, r]]
        rcons = R.restrict isCon trs
        stable = S.fromList
            [x | x <- S.toList sides, all isApp (S.toList $ R.succ trs x)]
        stabilizable0 = S.fromList
            [App l' r' |
            App l r <- S.toList stable,
            Con l' <- S.toList $ R.pred trs (Con l),
            Con r' <- S.toList $ R.pred trs (Con r),
            App l' r' `S.member` sides]
        add new old | S.null new = old
        add new old =
            let next = old `S.union` S.fromList
                    [App l r |
                    App l r <- S.toList sides,
                    not $ S.null $ S.intersection old $
                        R.succ trs (Con l) `S.union` R.succ trs (Con r)]
            in  add (next `S.difference` old) next
    in  add stabilizable0 S.empty

-- top rewrite steps
topsteps :: Ord f =>
    R.Rel (Side f) -> S.Set (Side f) -> Side f -> ([Tops f], [Tops f], [Tops f])
topsteps trs stabilizables s =
    let step2 (Con f) = [Con' f]
        step2 (App l r) = liftM2 App'
            (S.toList $ R.succ trs (Con l))
            (S.toList $ R.succ trs (Con r))
        tops = S.toList $ S.fromList $ S.toList (R.succ trs s) >>= step2
        left = [App' l r | App' l r <- tops, l `S.member` stabilizables]
        right = [App' l r | App' l r <- tops, r `S.member` stabilizables]
    in  (tops, left, right)

joinable :: (Ord f, PPretty f) =>
    R.Rel (Side f) -> R.Rel (Side f) -> Tops f -> Tops f -> Bool
joinable trs joins s t =
    -- (\res -> traceShow ("joinable" <+> ppretty s <+> ppretty t <+> text (show res) <$> ppretty s' <$> ppretty t' <$> ppretty s'' <$> ppretty t'') res) $
    any (`R.member` joins) (liftM2 (,) s' t') ||
    any (\((sl, sr), (tl, tr)) -> (sl, tl) `R.member` joins &&
         (sr, tr) `R.member` joins) (liftM2 (,) s'' t'')
  where
    succ' = S.toList . R.succ trs
    s' = case s of
        Con' c -> [Con c]
        App' l r -> [App l' r' | Con l' <- succ' l, Con r' <- succ' r]
    t' = case t of
        Con' c -> [Con c]
        App' l r -> [App l' r' | Con l' <- succ' l, Con r' <- succ' r]
    s'' = case s of
        Con' c -> [(Con l', Con r') | App l' r' <- succ' (Con c)]
        App' l r -> [(l, r)] ++ [(Con l', Con r') | App l' r' <- s' >>= succ']
    t'' = case t of
        Con' c -> [(Con l', Con r') | App l' r' <- succ' (Con c)]
        App' l r -> [(l, r)] ++ [(Con l', Con r') | App l' r' <- t' >>= succ']

reachable :: Ord f => R.Rel (Side f) -> Tops f -> Tops f -> Bool
reachable trs s t =
    any (`R.member` trs) (liftM2 (,) s' t') ||
    any (\((sl, sr), (tl, tr)) -> (sl, tl) `R.member` trs &&
         (sr, tr) `R.member` trs) (liftM2 (,) s'' t'')
  where
    succ' = S.toList . R.succ trs
    pred' = S.toList . R.pred trs
    s' = case s of
        Con' c -> [Con c]
        App' l r -> [App l' r' | Con l' <- succ' l, Con r' <- succ' r]
    t' = case t of
        Con' c -> [Con c]
        App' l r -> [App l' r' | Con l' <- pred' l, Con r' <- pred' r]
    s'' = case s of
        Con' c -> [(Con l', Con r') | App l' r' <- succ' (Con c)]
        App' l r -> [(l, r)]
    t'' = case t of
        Con' c -> [(Con l', Con r') | App l' r' <- pred' (Con c)]
        App' l r -> [(l, r)]

-- Definition 18
cconds :: (Ord f, PPretty f) =>
    R.Rel (Side f) -> R.Rel (Side f) -> S.Set (Side f) -> Side f -> Bool
cconds trs joins stabilizables s =
    let (tops, left, right) = topsteps trs stabilizables s
        leftp = (`S.member` S.fromList left)
        rightp = (`S.member` S.fromList right)
        succ' = S.toList . R.succ trs
    in  -- 1. term in Topsteps(s) are pairwise joinable
        all (uncurry $ joinable trs joins)
            [(a, b) | a : bs <- tails tops, b <- bs] &&
        -- 2. l1 @ r1, l2 @ r2 in Left(s) (Right(s)) -> l1, l2 (r1, r2) joinable
        all (`R.member` joins)
            [(a, b) | (App' _ a):bs <- tails left, (App' _ b) <- bs] &&
        all (`R.member` joins)
            [(a, b) | (App' a _):bs <- tails left, (App' b _) <- bs] &&
        -- 3. Left(Right) /= {} ==> for all Tops, exists reachable Lef(Right)
        (null left ||
            and [or [reachable trs t t' | t' <- left] | t <- tops]) &&
        (null right ||
            and [or [reachable trs t t' | t' <- right] | t <- tops]) &&
        -- 4.
        (null left ||
            and [or [leftp (App' l' r) | l' <- succ' l] | App' l r <- right]) &&
        (null right ||
            and [or [rightp (App' l r') | r' <- succ' r] | App' l r <- left])

-- Definition 24
djconds :: (Ord f, PPretty f) => R.Rel (Side f) -> R.Rel (Side f) ->
    S.Set (Side f) -> Side f -> Side f -> Bool
djconds trs joins stabilizables s t =
    let (topss, lefts, rights) = topsteps trs stabilizables s
        (topst, leftt, rightt) = topsteps trs stabilizables t
    in  -- 1. Topsteps(s) and Topsteps(t) are joinable
        all (uncurry $ joinable trs joins) [(a, b) | a <- topss, b <- topst] &&
        -- 2.
        (null lefts == null leftt) && (null rights == null rightt) &&
        -- 3.
        all (`R.member` joins)
            [(a, b) | (App' _ a) <- lefts, (App' _ b) <- leftt] &&
        -- 4.
        all (`R.member` joins)
            [(a, b) | (App' a _) <- rights, (App' b _) <- rightt]

-- "not deeply joinable"
ndj :: (Ord f, PPretty f) =>
    R.Rel (Side f) -> R.Rel (Side f) -> S.Set (Side f) -> R.Rel (Side f)
ndj trs joins stabilizables =
    let sides = S.toList $ S.fromList [x | (l, r) <- R.toList trs, x <- [l, r]]
        ndj0 = R.fromList [(s, s') | l@(s:_) <- tails sides, s' <- l,
            not $ djconds trs joins stabilizables s s']
        add new old | R.null new = old
        add new old =
            let next = old `R.union` new
                next' = R.fromList [
                    (s, t) | s <- sides, t <- sides,
                    let (_, lefts, rights) = topsteps trs stabilizables s,
                    let (_, leftt, rightt) = topsteps trs stabilizables t,
                    any (`R.member` next)
                        [(l, l') | App' l _ <- lefts, App' l' _ <- leftt] ||
                    any (`R.member` next)
                        [(r, r') | App' _ r <- rights, App' _ r' <- rightt]]
            in  add (next' `R.difference` next) next'
    in  add ndj0 R.empty

instance PPretty f => PPretty (Con f) where
    ppretty (F f a) = ppretty f <> "/" <> text (show a)
    ppretty (C i) = "c_" <> text (show i)

instance PPretty f => PPretty (Side f) where
    ppretty (Con f) = ppretty f
    ppretty (App f g) = "(" <> ppretty f <+> "@" <+> ppretty g <> ")"

instance PPretty f => PPretty (Tops f) where
    ppretty (Con' f) = ppretty f
    ppretty (App' f g) = "(" <> ppretty f <+> "@" <+> ppretty g <> ")"

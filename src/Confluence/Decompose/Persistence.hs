{-# LANGUAGE OverloadedStrings #-}
module Confluence.Decompose.Persistence (
    decompose,
) where

import Framework.Explain
import Util.Pretty
import Confluence.Types

import qualified Data.Rewriting.Rules as Rs
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Term as T
import Data.Rewriting.Rules ()
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Term (Term (..))

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Graph
import Data.Tree
import Data.Maybe
import Control.Arrow ((***))
import Control.Monad
import Text.PrettyPrint.ANSI.Leijen ((<+>), hcat, punctuate)

-- use unordered sorts.
unordered :: Bool
unordered = True

decompose :: (PPretty f, PPretty v, Ord f, Ord v) =>
    Problem f v -> Explain [Problem f v]
decompose trs = section "Find persistent decomposition for the TRS" $ do
    tell (ppretty trs)
    let trs' = relabel trs
    let (signature, order, attach) = attachment trs'
    section "Find most general sort attachment." $ do
        tell $ "using order" <+> (ppretty $ map Prec $ reverse $ edges order)
        tell $ "and types"
        tell $ ppretty signature
    tell $ "The maximal sorts are" <+> ppretty (sources order)

    section "Finding induced TRSs" $ do
        forM (sources order) $ \s -> do
            let sorts = S.fromList . flatten . head . dfs order $ [s]
                trs'' = [r | (r', r) <- zip trs' trs, attach (top (lhs r')) `S.member` sorts]
            section ("Sort" <+> ppretty s <+> "induces the TRS") $ do
                tell $ ppretty trs''
            return trs''

-- find most general ordered sort attachment for given TRS.
attachment :: Ord f => [Rule f Int] -> ([Type f], Graph, Node f -> Vertex)
attachment trs =
    let fs = S.unions $ map (S.fromList . R.funs) trs
        (gr, fromNode) = constraints trs
        (gr', toSort) = compress gr
        toSort' = (fmap (toSort M.!) fromNode M.!)
        signature = map getType (S.toList fs)
        getType f = Type f (map toSort' $ takeWhile (`M.member` fromNode) $ map (NArg f) [0..]) (toSort' (NRes f))
    in  (signature, gr', toSort')

relabel :: (Ord f, Ord v) => [Rule f v] -> [Rule f Int]
relabel trs = let
    -- translate function symbols to index
    -- fs = S.unions $ map (S.fromList . R.funs) trs
    -- fsm = M.fromList $ zip (S.toList fs) [0..]
    -- translate variables to index, rule by rule
    vss = map (S.fromList . R.vars) trs
    vis = scanl (+) 0 (map S.size vss)
    -- process a singe rule given variables, variable starting index and rule
    single vs i r = mapRule m r where
        m = T.map (vsm M.!) id {- (fsm M.!) -}
        vsm = M.fromList $ zip (S.toList vs) [i..]
  in
    zipWith3 single vss vis trs

mapRule :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
mapRule f (Rule lhs rhs) = Rule (f lhs) (f rhs)

data Node f
    = NVar Int
    | NArg f Int
    | NRes f
    deriving (Show, Ord, Eq)

-- build graph of contraints in the relabeled TRS.
-- each edge a -> b corresponds the a contraint a >= b or b |> a.
constraints :: Ord f => [Rule f Int] -> (Graph, M.Map (Node f) Vertex)
constraints trs =
    let fs = S.unions $ map (S.fromList . R.funs) trs

        root = [(top lhs, top rhs) | Rule lhs rhs <- trs] ++
               [(top rhs, top lhs) | unordered, Rule lhs rhs <- trs]
        internal = trs >>= \(Rule lhs rhs) -> int True lhs ++ int False rhs
        subterm = [(NRes f, n) | f <- S.toList fs,
                   n <- takeWhile (`S.member` nodes) $ map (NArg f) [0..]]
        constraints = root ++ internal ++ subterm

        nodes = S.fromList $ (root ++ internal) >>= \(a, b) -> [a, b]
        invM  = M.fromList $ zip (S.toList nodes) [0..]
        edges = [(invM M.! a, invM M.! b) | (a, b) <- constraints]
    in  (buildG (0, S.size nodes - 1) edges, invM)

int :: Bool -> Term f Int -> [(Node f, Node f)]
int strict (Var v) = []
int strict (Fun f as) = concat $ zipWith (go f) [0..] as where
    go f i (Var v) = [(NArg f i, NVar v)] ++ [(NVar v, NArg f i) | strict || unordered]
    go f i t = [(NArg f i, top t)] ++ [(top t, NArg f i) | unordered] ++ int strict t

top :: Term f Int -> Node f
top (Var v)   = NVar v
top (Fun f _) = NRes f

-- Graph utilities

-- compress the graph according to its strongly connected components
compress :: Graph -> (Graph, M.Map Vertex Vertex)
compress gr =
    let sccs = map flatten $ scc gr
        trans = M.fromList $ concat $ zipWith (map . flip (,)) [0..] sccs
        gr' = buildG (0, length sccs - 1)
           [(trans M.! a, trans M.! b) | (a, b) <- edges gr]
    in  (simplify gr', trans)

-- helper for compress: remove superfluous edges
simplify :: Graph -> Graph
simplify gr =
    let -- erase loops and duplicate edges
        gr1 = map simp $ A.assocs gr
        simp (i, ns) = (i, S.delete i . S.fromList $ ns)
        gr2 = A.array (A.bounds gr) (map trans gr1)
        trans (i, ns) = (i, S.unions $ (ns :) $ map (gr2 A.!) $ S.toList ns)
        gr3 = map step gr1
        step (i, ns) = (i, S.unions $ map (gr2 A.!) $ S.toList ns)
        gr4 = A.array (A.bounds gr) $ zipWith erase gr1 gr3
        erase (i, ns) (_, ns') = (i, S.toList $ ns `S.difference` ns')
    in  gr4

-- find sources
sources :: Graph -> [Vertex]
sources = sinks . transposeG

sinks :: Graph -> [Vertex]
sinks gr = [v | v <- vertices gr, null (gr A.! v)]

newtype Prec = Prec (Int, Int)

instance PPretty Prec where
   ppretty (Prec (a, b)) = ppretty a <+> ">" <+> ppretty b

data Type f = Type f [Int] Int

instance PPretty f => PPretty (Type f) where
    ppretty (Type f args res) =
        ppretty f <+> ":" <+> hcat (punctuate " x " $ map ppretty args) <+> "->" <+> ppretty res
    pprettyList = vList

{-
import Debug.Trace

showTRS :: (Show f, Show v) => [Rule f v] -> String
showTRS = showTPDB . map (mapRule (mapFV (('f':) . show) (('v':) . show)))
-}

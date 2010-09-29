{-# LANGUAGE OverloadedStrings #-}
module Confluence.Decompose.Persistence (
    decompose,
) where

import Framework.Explain
import Util.Pretty
import Confluence.Types

import Data.Termlib.TRS
import Data.Termlib.Rule
import Data.Termlib.Term hiding (pretty)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Graph
import Data.Tree
import Data.Maybe
import Control.Arrow ((***))
import Control.Monad
import Text.PrettyPrint.HughesPJ ((<+>), hcat, punctuate)

-- use unordered sorts.
unordered :: Bool
unordered = True

decompose :: (Pretty f, Pretty v, Ord f, Ord v) =>
    Problem f v -> Explain [Problem f v]
decompose trs = section "Find persistent decomposition for the TRS" $ do
    tell (pretty trs)
    let trs' = relabel trs
    let (signature, order, attach) = attachment trs'
    section "Find most general sort attachment." $ do
        tell $ "using order" <+> (pretty $ map Prec $ reverse $ edges order)
        tell $ "and types"
        tell $ pretty signature
    tell $ "The maximal sorts are" <+> pretty (sources order)

    section "Finding induced TRSs" $ do
        forM (sources order) $ \s -> do
            let sorts = S.fromList . flatten . head . dfs order $ [s]
                trs'' = [r | (r', r) <- zip trs' trs, attach (top (lhs r')) `S.member` sorts]
            section ("Sort" <+> pretty s <+> "induces the TRS") $ do
                tell $ pretty trs''
            return trs''

-- find most general ordered sort attachment for given TRS.
attachment :: Ord f => TRS f Int -> ([Type f], Graph, Node f -> Vertex)
attachment trs =
    let fs = S.unions $ map funs $ trs >>= \r -> [lhs r, rhs r]
        (gr, fromNode) = constraints trs
        (gr', toSort) = compress gr
        toSort' = (fmap (toSort M.!) fromNode M.!)
        signature = map getType (S.toList fs)
        getType f = Type f (map toSort' $ takeWhile (`M.member` fromNode) $ map (NArg f) [0..]) (toSort' (NRes f))
    in  (signature, gr', toSort')

relabel :: (Ord f, Ord v) => TRS f v -> TRS f Int
relabel trs = let
    -- translate function symbols to index
    fs = S.unions $ map funs $ trs >>= \r -> [lhs r, rhs r]
    fsm = M.fromList $ zip (S.toList fs) [0..]
    -- translate variables to index, rule by rule
    vss = map (\r -> vars (lhs r) `S.union` vars (rhs r)) trs
    vis = scanl (+) 0 (map S.size vss)
    -- process a singe rule given variables, variable starting index and rule
    single vs i r = mapRule m r where
        m = mapFV id {- (fsm M.!) -} (vsm M.!)
        vsm = M.fromList $ zip (S.toList vs) [i..]
  in
    zipWith3 single vss vis trs

data Node f
    = NVar Int
    | NArg f Int
    | NRes f
    deriving (Show, Ord, Eq)

-- build graph of contraints in the relabeled TRS.
-- each edge a -> b corresponds the a contraint a >= b or b |> a.
constraints :: Ord f => TRS f Int -> (Graph, M.Map (Node f) Vertex)
constraints trs =
    let fs = S.unions $ map funs $ trs >>= \r -> [lhs r, rhs r]

        root = [(top lhs, top rhs) | lhs :--> rhs <- trs] ++
               [(top rhs, top lhs) | unordered, lhs :--> rhs <- trs]
        internal = trs >>= \(lhs :--> rhs) -> int True lhs ++ int False rhs
        subterm = [(NRes f, n) | f <- S.toList fs,
                   n <- takeWhile (`S.member` nodes) $ map (NArg f) [0..]]
        constraints = root ++ internal ++ subterm

        nodes = S.fromList $ (root ++ internal) >>= \(a, b) -> [a, b]
        invM  = M.fromList $ zip (S.toList nodes) [0..]
        edges = [(invM M.! a, invM M.! b) | (a, b) <- constraints]
    in  (buildG (0, S.size nodes - 1) edges, invM)

int :: Bool -> Term f Int -> [(Node f, Node f)]
int strict (Var v) = []
int strict (App f as) = concat $ zipWith (go f) [0..] as where
    go f i (Var v) = [(NArg f i, NVar v)] ++ [(NVar v, NArg f i) | strict || unordered]
    go f i t = [(NArg f i, top t)] ++ [(top t, NArg f i) | unordered] ++ int strict t

top :: Term f Int -> Node f
top (Var v)   = NVar v
top (App f _) = NRes f

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

instance Pretty Prec where
   pretty (Prec (a, b)) = pretty a <+> ">" <+> pretty b

data Type f = Type f [Int] Int

instance Pretty f => Pretty (Type f) where
    pretty (Type f args res) =
        pretty f <+> ":" <+> hcat (punctuate " x " $ map pretty args) <+> "->" <+> pretty res
    prettyList = vList

{-
import Debug.Trace

showTRS :: (Show f, Show v) => TRS f v -> String
showTRS = showTPDB . map (mapRule (mapFV (('f':) . show) (('v':) . show)))
-}

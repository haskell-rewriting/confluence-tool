-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Development where

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Substitution.Type
import Data.Rewriting.Substitution
import Data.Rewriting.Rules hiding (map)
import Data.Rewriting.Rule (Rule (..))

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

data TermTree f v = VarT v | FunT f [TermForest f v]
    deriving Show

type TermForest f v = [TermTree f v]

mapFVT :: (f -> f') -> (v -> v') -> TermForest f v -> TermForest f' v'
mapFVT g h = fmap mapFVT' where
    mapFVT' (VarT v) = VarT (h v)
    mapFVT' (FunT f ts) = FunT (g f) (fmap (fmap mapFVT') ts)

fromTerm :: Term f v -> TermTree f v
fromTerm (Var v) = VarT v
fromTerm (Fun f ts) = FunT f (map ((:[]) . fromTerm) ts)

classify :: (Ord f, Ord v)
    =>TermForest f v -> (S.Set v, M.Map f [[TermForest f v]])
classify ts =
    let vs = [v | VarT v <- ts]
        fs = [(f, [ts']) | FunT f ts' <- ts]
    in  (S.fromList vs, M.fromListWith (++) fs)

intersect :: (Show f, Show v, Ord f, Ord v)
    => TermForest f v -> TermForest f v -> TermForest f v
intersect xs ys =
    let (vx, fx) = classify xs
        (vy, fy) = classify ys
        (vz, fz) = (S.intersection vx vy,
                    M.intersectionWith (liftM2 (zipWith intersect)) fx fy)
    in  [VarT v | v <- S.toList vz] ++
        [FunT f ts | (f, tss) <- M.toList fz, ts <- tss]

toList :: TermForest f v -> [Term f v]
toList ts = ts >>= toListT where
    toListT (VarT v) = [Var v]
    toListT (FunT f ts) = [Fun f ts' | ts' <- mapM toList ts]

simplify :: TermForest f v -> TermForest f v
simplify = concatMap simplify' where
    simplify' (VarT v) = [VarT v]
    simplify' (FunT f ts) = let ts' = map simplify ts
                            in  [FunT f ts | all (not . null) ts]

-- | parallel reductions
parallel :: (Eq f, Eq v, Ord v') => [Rule f v'] -> Term f v -> TermForest f v
parallel trs = simplify . go where
    go (Var v) = [VarT v]
    go t@(Fun f ts) = [FunT f (map go ts)] ++ map fromTerm (rootRewrites trs t)

-- | complete developments
development :: (Eq f, Eq v, Ord v') => [Rule f v'] -> Term f v -> TermForest f v
development trs = simplify . go where
    go (Var v) = [VarT v]
    go t@(Fun f ts) = [FunT f (map go ts)] ++ concat [apply r t | r <- trs]
    apply (Rule l r) t = case match l t of
        Nothing -> []
        Just s -> subst (fmap go (toMap s)) r
    subst s (Var v) = s M.! v
    subst s (Fun f ts) = [FunT f (fmap (subst s) ts)]

rootRewrites trs = map result . rootRewrite trs

-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Util.Relation (
    Rel,
    member,
    empty,
    insert,
    union,
    difference,
    pred,
    succ,
    null,
    restrict,
    fromList,
    toList,
) where

import Prelude hiding (pred, succ, elem, null)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')
import Control.Monad

data Rel a = Rel { suc :: M.Map a (S.Set a), pre :: M.Map a (S.Set a) }

member :: Ord a => (a, a) -> Rel a -> Bool
member (a, b) rel = maybe False (S.member b) $ a `M.lookup` suc rel

insert :: Ord a => (a, a) -> Rel a -> Rel a
insert (a, b) rel = Rel{ suc = insert' a b (suc rel),
                         pre = insert' b a (pre rel) }
  where
    insert' a b = M.alter (Just . maybe (S.singleton b) (S.insert b)) a

empty :: Rel a
empty = Rel{ suc = M.empty, pre = M.empty }

succ :: Ord a => Rel a -> a -> S.Set a
succ rel a = maybe S.empty id $ a `M.lookup` suc rel

pred :: Ord a => Rel a -> a -> S.Set a
pred rel a = maybe S.empty id $ a `M.lookup` pre rel

union :: Ord a => Rel a -> Rel a -> Rel a
union rel rel' = Rel{ suc = union' (suc rel) (suc rel'),
                      pre = union' (pre rel) (pre rel') }
  where
    union' a b = M.unionWith (S.union) a b

difference :: Ord a => Rel a -> Rel a -> Rel a
difference rel rel' = Rel{ suc = difference' (suc rel) (suc rel'),
                           pre = difference' (pre rel) (pre rel') }
  where
    difference' = M.differenceWith
        (\a b -> let r = S.difference a b in guard (S.null r) >> return r)

null :: Ord a => Rel a -> Bool
null rel = M.foldr (\a b -> S.null a && b) True (suc rel)

fromList :: Ord a => [(a, a)] -> Rel a
fromList = foldl' (flip insert) empty

toList :: Ord a => Rel a -> [(a, a)]
toList rel = [(a, b) | (a, bs) <- M.toList (suc rel), b <- S.toList bs]

restrict :: Ord a => (a -> Bool) -> Rel a -> Rel a
restrict dom rel = Rel{ suc = restrict' (suc rel), pre = restrict' (pre rel) }
  where
    restrict' = M.map (S.filter dom) . (M.filterWithKey (const . dom))

-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances #-}
module Framework.Explain (
    Explain,
    explanation,
    result,
    explain,
    tell,
--    render,
    runExplain,
    section,
) where

import Control.Monad.RWS
import Control.Applicative
import Control.Arrow (second)
import Data.List

import Text.PrettyPrint.ANSI.Leijen as P

data State = State {
    cSection :: Int
}

data Context = Context {
    cSections :: [Int]
}

data MDoc = Empty | MDoc Doc

unMDoc :: MDoc -> Doc
unMDoc Empty = P.empty
unMDoc (MDoc doc) = doc

newtype Explain a = Explain {
    runExplain' :: RWS Context MDoc State a
} deriving (Monad, Functor, Applicative)

instance MonadWriter Doc Explain where
    tell = Explain . tell . MDoc
    listen = Explain . fmap (\(a, b) -> (a, unMDoc b)) . listen . runExplain'
    pass = Explain . pass . fmap (\(a, f) -> (a, MDoc . f . unMDoc)) . runExplain'

instance Monoid MDoc where
    mempty = Empty
    mappend (MDoc a) (MDoc b) = MDoc (a P.<$> b)
    mappend a Empty = a
    mappend Empty b = b

{-
instance Monoid w => Applicative (RWS r w s) where
    pure = return
    (<*>) = ap
-}

runExplain :: Explain a -> (a, Doc)
runExplain m = (a, w P.<$> P.empty) where
    (a, _, MDoc w) = runRWS (runExplain' m) c0 s0
    c0 = Context { cSections = [] }
    s0 = State { cSection = 0 }

explanation :: Explain a -> Doc
explanation = snd . runExplain

result :: Explain a -> a
result = fst . runExplain

explain :: String -> Explain ()
explain = tell . text

section :: Doc -> Explain a -> Explain a
section doc t = Explain $ do
    st@State{ cSection = s0 } <- get
    let s = succ s0
    ct@Context{ cSections = ss } <- ask
    let label = concat $ intersperse "." $ map show $ reverse (s : ss)
    tell $ MDoc $ text label P.<+> doc
    r <- local (const (ct{ cSections = s:ss })) $ do
         put st{ cSection = 0 }
         runExplain' t
    put st{ cSection = s }
    return r

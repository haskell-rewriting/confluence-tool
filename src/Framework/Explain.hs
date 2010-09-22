{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Framework.Explain (
    Explain,
    explanation,
    result,
    explain,
    tell,
    render,
    runExplain,
    section,
) where

import Control.Monad.RWS
import Control.Applicative
import Control.Arrow (second)
import Data.List

import Text.PrettyPrint.HughesPJ as P

data State = State {
    cSection :: Int
}

data Context = Context {
    cSections :: [Int]
}

newtype Explain a = Explain {
    runExplain' :: RWS Context Doc State a
} deriving (Monad, Functor, Applicative, MonadWriter Doc)

instance Monoid Doc where
    mempty = P.empty
    mappend = ($+$)

instance Monoid w => Applicative (RWS r w s) where
    pure = return
    (<*>) = ap

runExplain :: Explain a -> (a, Doc)
runExplain m = (a, w) where
    (a, _, w) = runRWS (runExplain' m) c0 s0
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
    tell $ text label <+> doc
    r <- local (const (ct{ cSections = s:ss })) $ do
         put st{ cSection = 0 }
         runExplain' t
    put st{ cSection = s }
    return r

{-
instance MonadWriter Doc Explain where
    tell = Explain . tell . VCat
    listen = Explain . fmap (second unVCat) . listen . runExplain
    pass = Explain . pass . fmap (second (\f -> VCat . f . unVCat)) . runExplain

newtype VCat = VCat { unVCat :: Doc }

instance Monoid VCat where
    mempty = VCat (P.empty)
    mappend a b = VCat $ unVCat a $+$ unVCat b
-}

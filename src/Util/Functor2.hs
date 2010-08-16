module Util.Functor2 (
    Functor2 (..)
) where

class Functor2 f where
    fmap2 :: (a -> b) -> f a c -> f b c

instance Functor2 (,) where
    fmap2 f (a, b) = (f a, b)

instance Functor2 ((,,) a) where
    fmap2 f (a, b, c) = (a, f b, c)

instance Functor2 ((,,,) a b) where
    fmap2 f (a, b, c, d) = (a, b, f c, d)

instance Functor2 Either where
    fmap2 f (Left  a) = Left  (f a)
    fmap2 f (Right b) = Right b
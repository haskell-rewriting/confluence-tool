{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Util.Pretty (
    PPretty (..),
    hList,
    vList,
) where

import qualified Data.Rewriting.Rules as Rules
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Term as Term
import Data.Rewriting.Term (Term (..))

import Text.PrettyPrint.ANSI.Leijen
import GHC.Exts (IsString(..))

class PPretty a where
    ppretty :: a -> Doc
    pprettyList :: [a] -> Doc
    pprettyList = hList

instance PPretty a => PPretty [a] where
    ppretty = pprettyList
    pprettyList = vList

instance (PPretty a, PPretty b) => PPretty (Either a b) where
    ppretty (Left a) = ppretty a <> "l"
    ppretty (Right b) = ppretty b <> "r"

instance (PPretty a, PPretty b) => PPretty (a, b) where
    ppretty (a, b) = "(" <> ppretty a <> "," <+> ppretty b <> ")"

instance (PPretty a, PPretty b, PPretty c) => PPretty (a, b, c) where
    ppretty (a, b, c) = "(" <> ppretty a <> "," <+> ppretty b <> "," <+> ppretty c <> ")"

newtype S a = S { unS :: a }

instance Show a => PPretty (S a) where
    ppretty = text . show . unS

instance PPretty Integer where ppretty = ppretty . S
instance PPretty Int where ppretty = ppretty . S
instance PPretty Bool where ppretty = ppretty . S

instance PPretty Char where
    ppretty = ppretty . S
    pprettyList = text -- ppretty . S

instance (PPretty f, PPretty v) => PPretty (Rule f v) where
    ppretty (Rule lhs rhs) = fillSep [ppretty lhs, "-->", ppretty rhs]
    pprettyList = vList

instance (PPretty f, PPretty v) => PPretty (Term f v) where
    ppretty (Var v) = "?" <> ppretty v
    ppretty (Fun f []) = ppretty f
    ppretty (Fun f as) = fillCat [ppretty f, "(", fillSep . punctuate "," $ map ppretty as, ")"]

hList :: PPretty a => [a] -> Doc
hList [] = "[]"
hList xs = hcat
    ["[",
    fillSep . punctuate "," . map ppretty $ xs,
    "]"]

vList :: PPretty a => [a] -> Doc
vList xs = vcat
    ["[",
    indent 4 . vcat . punctuate "," . map ppretty $ xs,
    "]"]

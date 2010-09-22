{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module Util.Pretty (
    Pretty (..),
    hList,
    vList,
) where

import Data.Termlib.TRS
import Data.Termlib.Term hiding (pretty)

import Text.PrettyPrint.HughesPJ
import GHC.Exts (IsString(..))

instance IsString Doc where
    fromString = text

class Pretty a where
    pretty :: a -> Doc
    prettyList :: [a] -> Doc
    prettyList = hList

instance Pretty a => Pretty [a] where
    pretty = prettyList
    prettyList = vList

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = "(" <> pretty a <> "," <+> pretty b <> ")"

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = "(" <> pretty a <> "," <+> pretty b <> "," <+> pretty c <> ")"

newtype S a = S { unS :: a }

instance Show a => Pretty (S a) where
    pretty = text . show . unS

instance Pretty Integer where pretty = pretty . S
instance Pretty Int where pretty = pretty . S
instance Pretty Bool where pretty = pretty . S

instance Pretty Char where
    pretty = pretty . S
    prettyList = text -- pretty . S

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty (lhs :--> rhs) = fsep [pretty lhs, "-->", pretty rhs]
    prettyList = vList

instance (Pretty f, Pretty v) => Pretty (Term f v) where
    pretty (Var v) = "?" <> pretty v
    pretty (App f []) = pretty f
    pretty (App f as) = fcat [pretty f, "(", fsep . punctuate "," $ map pretty as, ")"]

hList :: Pretty a => [a] -> Doc
hList [] = "[]"
hList xs = hcat
    ["[",
    fsep . punctuate "," . map pretty $ xs,
    "]"]

vList :: Pretty a => [a] -> Doc
vList xs = vcat
    ["[",
    nest 4 . vcat . punctuate "," . map pretty $ xs,
    "]"]

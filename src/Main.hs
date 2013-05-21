-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

module Main (main) where

import Confluence.Decompose.Persistence
import qualified Confluence.Direct.KnuthBendix as K
import qualified Confluence.Direct.HuetToyamaOostrom as H
import qualified Confluence.Direct.Ground as G
import Termination.TTT2
import Framework.Explain
import Framework.Types

import qualified Data.Rewriting.Problem as P

import Text.PrettyPrint.ANSI.Leijen
import System.Environment
import Control.Monad

main = do
    [f] <- getArgs
    p <- P.parseFileIO f
    let trs = P.allRules (P.rules p)
    let dec = decompose trs
        ds = result dec
    putDoc $ explanation dec
    foldr next (return ()) [
        return $ G.confluent trs,
        K.confluent trs,
        return $ H.confluent trs
        ]

next :: IO (Explain Answer) -> IO () -> IO ()
next a b = do
    a' <- a
    putDoc $ empty <$> explanation a'
    case result a' of
        Maybe -> b
        _ -> print $ result a'

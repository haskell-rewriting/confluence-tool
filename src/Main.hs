module Main (main) where

import Confluence.Decompose.Persistence
import qualified Confluence.Direct.KnuthBendix as K
import qualified Confluence.Direct.HuetToyamaOostrom as H
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
    foo <- K.confluent trs
    putDoc $ explanation foo
    if result foo == Maybe then do
        let dc = H.confluent trs
        putDoc $ explanation dc
        print $ result dc
     else do
        print $ result foo

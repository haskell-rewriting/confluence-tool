module Main (main) where

import Confluence.Decompose.Persistence
import qualified Confluence.Direct.KnuthBendix as K
import qualified Confluence.Direct.HuetToyamaOostrom as H
import Termination.TTT2
import Framework.Explain
import Framework.Types

import Data.Termlib.TRS.TPDB

import System.Environment
import Control.Monad

main = do
    [f] <- getArgs
    c <- readFile f
    trs <- either error return $ readTPDB c
    let dec = decompose trs
        ds = result dec
    putStrLn $ render $ explanation dec
    foo <- K.confluent trs
    putStrLn $ render $ explanation foo
    if result foo == Maybe then do
        let dc = H.confluent trs
        putStrLn $ render $ explanation dc
        print $ result dc
     else do
        print $ result foo

module Main (main) where

import Confluence.Decompose.Persistence
import Termination.TTT2
import Framework.Explain

import Data.Termlib.TRS.TPDB

import System.Environment
import Control.Monad

main = do
    [f] <- getArgs
    c <- readFile f
    trs <- either error return $ readTPDB c
    let dec = decompose trs
        ds = result dec
        ex = explanation dec
    putStrLn $ render ex
    ttt2 trs >>= putStrLn . ("TTT2 says, " ++) . show

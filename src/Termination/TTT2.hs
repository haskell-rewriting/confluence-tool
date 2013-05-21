-- This file is covered by an MIT license. See 'LICENSE' for details.
-- Author: Bertram Felgenhauer

{-# LANGUAGE OverloadedStrings #-}
module Termination.TTT2 (
    ttt2,
) where

import Termination.Types
import Framework.Types

import qualified Data.Rewriting.Problem as P
import qualified Data.Rewriting.Rules as Rs
import qualified Data.Rewriting.Rule as R
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Set as S

import System.Process
import System.IO
import Control.Exception

ttt2 :: Problem -> IO Answer
ttt2 problem = do
    let cmd = (proc "ttt2" ["-p", "-", ".trs"]){
            std_in = CreatePipe,
            std_out = CreatePipe
        }
    answer <- bracket
       (createProcess cmd)
       (\(_, _, _, pid) -> terminateProcess pid) $
       \(Just stdin, Just stdout, _, _) -> do
          -- putStrLn "Invoking TTT2"
          -- putStrLn $ showTPDB problem
          hPutStr stdin $ showTPDB problem
          hClose stdin
          hGetLine stdout
    case answer of
        "NO" -> return No
        "YES" -> return Yes
        "MAYBE" -> return Maybe

showTPDB :: Problem -> String
showTPDB problem = show $
    let vars = S.toList $ S.fromList $ Rs.vars problem
    in  "(VAR" <+> fillSep (map text vars) <> ")" <$>
        "(RULES" <$>
        indent 4 (vcat $ map (R.prettyRule "->" text text) problem) <$>
        ")"

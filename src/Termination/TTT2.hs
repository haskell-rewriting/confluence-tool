module Termination.TTT2 (
    ttt2,
) where

import Termination.Types
import Framework.Types

import Data.Termlib.TRS.TPDB

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
          putStrLn "Invoking TTT2"
          putStrLn $ showTPDB problem
          hPutStr stdin $ showTPDB problem
          hClose stdin
          hGetLine stdout
    case answer of
        "NO" -> return No
        "YES" -> return Yes
        "MAYBE" -> return Maybe

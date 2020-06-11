module Main where

import Lib
import System.Console.CmdArgs

main :: IO ()
main = cmdArgs option >>= run

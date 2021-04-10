module Main where

import System.Console.Haskeline
-- import Control.Monad.Trans
import Parsing
import Expr
import REPL

main :: IO ()
main = runInputT defaultSettings (repl initState)


module Main where

import System.Console.Haskeline
import Data.List (isPrefixOf)
import Control.Monad.State.Strict (get, runStateT, StateT)

-- import Control.Monad.Trans
import Parsing
import Expr
import REPL

-- Copied from https://www.reddit.com/r/haskell/comments/1os0yq/haskeline_woes/
-- Author: TheBB
findCompletion :: String -> StateM [Completion]
findCompletion s = do map simpleCompletion . filter (s `isPrefixOf`) . wordList <$> get

hlSettings :: Settings (StateT State IO)
hlSettings = setComplete (completeWord Nothing " \t" findCompletion) defaultSettings

main :: IO ((), State)
main = runStateT (runInputT hlSettings repl) initState
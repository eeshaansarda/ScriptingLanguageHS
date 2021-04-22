module Test where

import Control.Monad (when)
import Control.Monad.State.Strict
import Control.Monad.Trans (lift)
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromJust)
import System.Console.Haskeline
import System.Console.Haskeline.Completion

type Namespace = [String]
type StateM = StateT Namespace IO
type InputM = InputT StateM

wordList :: [String]
wordList = ["False", "True", "else", "if", "import", "print", "quit", "then", "toFloat(", "toInt(", "toString(", "while"]

findCompletion :: String -> StateM [Completion]
findCompletion s = get >>= \ns -> return $ map simpleCompletion $ filter (s `isPrefixOf`) (ns ++ wordList)

hlSettings :: Settings (StateT Namespace IO)
hlSettings = setComplete (completeWord Nothing " \t" findCompletion) defaultSettings

repl :: InputT (StateT [String] IO) ()
-- repl :: InputM ()
repl = do
    inp <- getInputLine ">> "
    when (isJust inp) $ do
        ns <- lift get
        lift $ put ((fromJust inp) : ns)
    repl

test :: IO ((), Namespace)
test = runStateT (runInputT hlSettings repl) ["Abc"]
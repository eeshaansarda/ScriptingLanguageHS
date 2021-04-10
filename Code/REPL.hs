module REPL where

import System.Console.Haskeline
-- import Control.Monad.Trans
import Expr
import Parsing

data State = State {vars :: [(Name, Value)]}

initState :: State
initState = State []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name value [] = [(name, value)]
updateVars name value ((var, val) : vars)
  | name == var = (var, value) : vars
  | otherwise = (var, val) : updateVars name value vars

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = [(var, val) | (var, val) <- vars, var /= name]

process :: State -> Command -> InputT IO ()
process st (Set var e) =
  do
    case eval (vars st) e of
      Nothing -> repl st
      Just eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating e
        repl st'
process st (Print e) =
  do
    case eval (vars st) e of
      Nothing -> outputStrLn ("Invalid expression")
      Just eval_res -> do
        outputStrLn (show eval_res)
    -- Print the result of evaluation
    repl st
process st Quit
     = outputStrLn "Bye"

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> InputT IO ()
repl st = do outputStrLn ("Variables: " ++ show (vars st)) -- TODO: debug message, to be removed in the future
             inp <- getInputLine ("> ")
             -- inp <- getLine
             case inp of
                Nothing    -> return ()
                Just input -> 
                    case parse pCommand input of
                        [(cmd, "")] -> -- Must parse entire input
                                process st cmd
                        _ -> do outputStrLn "Parse error"
                                repl st

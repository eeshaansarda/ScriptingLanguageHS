module REPL where

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

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' = undefined
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Print e) 
     = do let st' = undefined
          -- Print the result of evaluation
          repl st'
process st Quit
     = putStrLn "Bye"

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr ("> ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st

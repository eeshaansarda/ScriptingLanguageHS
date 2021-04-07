module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)] }

initState :: State
initState = State []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars = undefined

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar = undefined

process :: State -> Command -> IO ()
process st (Set var e) 
     = do let st' = initState
          -- st' should include the variable set to the result of evaluating e
          putStrLn "in Set"
          repl st'
process st (Print e) 
     = do let st' = initState
          -- Print the result of evaluation
          putStrLn "in Print"
          repl st'

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

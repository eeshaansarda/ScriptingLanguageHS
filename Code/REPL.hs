module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

type StateM = StateT State IO
type InputM = InputT StateM
data State = State {vars :: BTree, commands :: [String], wordList :: [String]}

initState :: State
initState = State Leaf [] ["False", "True", "else", "if", "import", "print", "quit", "then", "toFloat(", "toInt(", "toString(", "while"]

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> BTree -> BTree
updateVars _name _value Leaf = Node (_name, _value) Leaf Leaf
updateVars _name _value (Node (name, value) ltree rtree)
  | _name < name = Node (name, value) (updateVars _name _value ltree) rtree
  | _name > name = Node (name, value) ltree (updateVars _name _value rtree)
  | otherwise = Node (name, _value) ltree rtree

-- Return a new set of variables with the given name removed
dropVar :: Name -> BTree -> BTree
dropVar _name Leaf = Leaf
dropVar _name (Node (name, value) ltree rtree)
  | _name < name = Node (name, value) (dropVar _name ltree) rtree
  | _name > name = Node (name, value) ltree (dropVar _name rtree)
  | otherwise = case (ltree, rtree) of
    (Leaf, _) -> rtree
    (_, Leaf) -> ltree
    _ -> Node (left_largest_var, left_largest_val) (dropVar left_largest_var ltree) rtree
    where (left_largest_var, left_largest_val) = largestVar ltree
          largestVar tree = case tree of -- using "last (inorderTraversal tree)" or something like that is against the purpose of using binary search tree.
            Node (name_, value_) _ Leaf -> (name_, value_)
            Node (_, _) _ rtree -> largestVar rtree

process :: State -> Command -> InputT StateM State
process st (Set var e) =
  do
    case eval (vars st) e of
      Nothing -> return st
      Just Input -> do inpVal <- getInputLine ("Input > ")
                       case inpVal of
                         Just inp -> return (st {vars = updateVars var (StrVal inp) (vars st)})
                         Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
      Just eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating e
        return st'
process st (Print e) =
  do
    case eval (vars st) e of
      Nothing -> outputStrLn ("Invalid expression")
      Just eval_res -> do
        outputStrLn (show eval_res)
    -- Print the result of evaluation
    return st
process st (If e b1 b2) = case eval (vars st) e of
  Just (BoolVal True)  -> do st' <- processBlock st b1
                             return st'
  Just (BoolVal False) -> do st' <- processBlock st b2
                             return st'
  _                    -> do outputStrLn "Invalid boolean value"
                             return st
process st (While e block) = loop st block e
  where loop :: State -> [Command] -> Expr -> InputT StateM State
        loop st cmds expr = case eval (vars st) expr of
          Just (BoolVal True)  -> do st' <- processBlock st cmds
                                     loop st' cmds expr
          Just (BoolVal False) -> return st
          _                    -> do outputStrLn "Invalid boolean value"
                                     return st

processBlock :: State -> [Command] -> InputT StateM State
processBlock st (cmd: [])   = do st' <- process st cmd
                                 return st'
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st


-- processRepl :: State -> Command -> InputT IO ()
-- processRepl st (Set var e)  = do st' <- process st (Set var e)
--                                  repl st'
-- processRepl st (Print e)    = do st' <- process st (Print e)
--                                  repl st'
-- processRepl st (Quit)       = outputStrLn "Bye"
-- processRepl st (Import filepath) = do text <- liftIO (readFile filepath)
--                                       repl (st {commands = lines text ++ commands st})
-- processRepl st (If e b1 b2) = case eval (vars st) e of
--   Just (BoolVal True)  -> do st' <- processBlock st b1
--                              repl st'
--   Just (BoolVal False) -> do st' <- processBlock st b2
--                              repl st'
--   _                    -> outputStrLn "Invalid boolean value"
-- processRepl st (While e block) = loop st block e
--   where loop :: State -> [Command] -> Expr -> InputT IO ()
--         loop st cmds expr = case eval (vars st) expr of
--           Just (BoolVal True)  -> do st' <- processBlock st cmds
--                                      loop st' cmds expr
--           Just (BoolVal False) -> repl st
--           _                    -> outputStrLn "Invalid boolean value"


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputM ()
repl = do st <- lift get
          outputStrLn ("Variables: " ++ show (vars st)) -- TODO: debug message, to be removed in the future
          inp <- case commands st of
            [] -> getInputLine ("> ")
            (x:xs) -> do lift $ put st {commands = xs}
                         return (Just x)
          -- inp <- getLine
          st <- lift get
          case inp of
            Nothing    -> return ()
            Just input -> 
                case parse pStatement input of
                    [(cmd, "")] -> -- Must parse entire input
                      case cmd of
                        Quit -> do outputStrLn "Bye"
                                   return ()
                        (Set var e) -> do st' <- process st cmd
                                          lift $ put st' {wordList = var : wordList st'}
                                          repl
                        (Import filepath) -> do text <- lift $ lift (readFile filepath)
                                                lift $ put st {commands = lines text ++ commands st}
                                                repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl

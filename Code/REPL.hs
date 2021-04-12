module REPL where

import System.Console.Haskeline
-- import Control.Monad.Trans
import Expr
import Parsing

data State = State {vars :: BTree}

initState :: State
initState = State Leaf

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

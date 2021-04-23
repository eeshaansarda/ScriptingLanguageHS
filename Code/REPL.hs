module REPL where

import System.Console.Haskeline
import Expr
import Parsing
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict (get, put, runStateT, StateT)

type StateM = StateT State IO
type InputM = InputT StateM
data State = State {vars :: BTree, commands :: [String],
                    functions :: [(Name, [Name], [Command])],
                    wordList :: [String]}

-- Functions that can be implemented in this "language"
-- The others will go direct into eval
-- no return as of now
initFunc :: [(Name, [Name], [Command])]
initFunc = [("printDouble", ["a"], [Print (Mul (Var "a") (Val (IntVal 2)))])]
            --(Fun "printNTimes", ["a", "n"], [Print (Mul (Var "a") 2)])]

initCompletionList :: [String]
initCompletionList = ["False", "True", "else", "if", "import", "print", "quit", "toFloat(", "toInt(", "toString(", "while", "fun", "printDouble("]

initState :: State
initState = State Leaf [] initFunc initCompletionList

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

updateFuns :: Name -> [Name] -> [Command] -> [(Name, [Name], [Command])] -> [(Name, [Name], [Command])]
updateFuns name _vars _commands [] = [(name, _vars, _commands)]
updateFuns name _vars _commands ((n, v, c): funs)
  | name == n = (name, _vars, _commands) : funs
  | otherwise = (n, v, c) : updateFuns name _vars _commands funs

process :: State -> Command -> InputT StateM State
process st (Set var e) =
  do
    case eval (vars st) e of
      Left (ExprErr op err_msg) -> do outputStrLn ("Error on " ++ op ++ ": " ++ err_msg)
                                      return st -- error
      Right Input -> do inpVal <- getInputLine ("Input > ")
                        case inpVal of
                         Just inp -> return (st {vars = updateVars var (StrVal inp) (vars st)})
                         Nothing -> return (st {vars = updateVars var (StrVal "") (vars st)})
      -- TODO: error message from here
      Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                      case val of
                                        Right eval_res -> return st {vars = updateVars var eval_res (vars st)}
                                        Left -> return st -- TODO: no function of name "name" exist error
      -- TODO: till here
      Right eval_res -> do
        let st' = st {vars = updateVars var eval_res (vars st)}
        -- st' should include the variable set to the result of evaluating e
        return st'
process st (Print e) =
  do
    case eval (vars st) e of

         Left (ExprErr op err_msg) -> do outputStrLn ("Error on " ++ op ++ ": " ++ err_msg)
         Right Input -> do inpVal <- getInputLine ("Input > ")
                           case inpVal of
                                Just inp -> do outputStrLn inp
                                Nothing -> do outputStrLn ""
         -- TODO: Handle error below
         Right (FunCall name exprs) -> do val <- funCallVal st name exprs
                                          case val of
                                            Right eval_res -> outputStrLn (show eval_res) -- how will it show a string
                                            Left -> outputStrLn "" -- error
         -- TODO till here
         Right eval_res -> do
           outputStrLn (show eval_res)
         return st

process st (If e b1 b2) = case eval (vars st) e of
  Right (BoolVal True)  -> do st' <- processBlock st b1
                              return st'
  Right (BoolVal False) -> do st' <- processBlock st b2
                              return st'
  _                    -> do outputStrLn "Invalid boolean value"
                             return st
process st (While e block) = loop st block e
  where loop :: State -> [Command] -> Expr -> InputT StateM State
        loop state cmds expr = case eval (vars state) expr of
          Right (BoolVal True)  -> do st' <- processBlock state cmds
                                      loop st' cmds expr
          Right (BoolVal False) -> return state
          _                    -> do outputStrLn "Invalid boolean value"
                                     return state
process st (Fun name vars commands) = return st {functions = updateFuns name vars commands (functions st)}
process st (VoidFunCall name exprs) = case fun of
  [] -> return st
  [(fname, vnames, commands)] -> if (length exprs == length vnames && blockIsVoid commands)
                                    then do sState <- assignValues scopedState vnames exprs
                                            processBlock sState commands
                                            return st
                                 else return st
  where scopedState :: State
        scopedState = st
        fun :: [(Name, [Name], [Command])]
        fun = filter (\(x, _, _) -> x == name) (functions st)
        assignValues :: State -> [Name] -> [Expr] -> InputT StateM State
        assignValues state (v:vs) (e:es) = do state' <- process state (Set v e)
                                              assignValues state' vs es
        assignValues state []     []     = return state

processBlock :: State -> [Command] -> InputT StateM State
processBlock st (cmd: [])   = do st' <- process st cmd
                                 return st'
processBlock st (cmd: cmds) = do st' <- process st cmd
                                 processBlock st' cmds
processBlock st _           = return st

processBlockRet :: (State, Either EvalError Expr) -> [Command] -> InputT StateM (State, Either EvalError Expr)
processBlockRet (st, _) (Return e: _)   = return (st, Right e)
processBlockRet (st, _) (cmd: [])   = do st' <- process st cmd
                                         return (st', Left (ExprErr "proessBlockRet" "TODO"))
processBlockRet (st, _) (cmd: cmds) = do st' <- process st cmd
                                         processBlockRet (st', Left (ExprErr "proessBlockRet" "TODO")) cmds
-- processBlockRet (st, _) _           = return st

blockIsVoid :: [Command] -> Bool
blockIsVoid []            = True
blockIsVoid (Return x: _) = False
blockIsVoid (x: xs)       = blockIsVoid xs

funCallVal :: State -> Name -> [Expr] -> InputT StateM (Maybe Value)
funCallVal st name exprs = case fun of
        [] -> return Nothing -- TODO no function of name "name"
        [(fname, vnames, commands)] -> if (length exprs == length vnames && not(blockIsVoid commands))
                                          then do sState <- assignValues scopedState vnames exprs
                                                  (st', e) <- processBlockRet (sState, Nothing) commands
                                                  case e of
                                                    Just expression -> return (eval (vars st') expression)
                                                    Nothing -> return Nothing
                                       else return Nothing -- TODO the number of arguments doesnt match OR does not contain a return statement
        where scopedState :: State
              scopedState = st
              fun :: [(Name, [Name], [Command])]
              fun = filter (\(x, _, _) -> x == name) (functions st)
              assignValues :: State -> [Name] -> [Expr] -> InputT StateM State
              assignValues state (v:vs) (e:es) = do state' <- process state (Set v e)
                                                    assignValues state' vs es
              assignValues state []     []     = return state

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: InputM ()
repl = do st <- lift get
          -- outputStrLn ("Variables: " ++ show (vars st)) -- TODO: debug message, to be removed in the future
          -- outputStrLn ("Functions: " ++ show (functions st)) -- TODO: debug message, to be removed in the future
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
                        (Fun name _ _)    -> do st' <- process st cmd
                                                lift $ put st' {wordList = (name ++ "(") : wordList st'}
                                                repl
                        _ -> do st' <- process st cmd
                                lift $ put st'
                                repl
                    _ -> do outputStrLn "Parse error"
                            repl

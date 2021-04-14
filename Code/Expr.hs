module Expr where

import Parsing

import Data.Maybe

type Name = String

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | ToString Expr

          | Val Value
          | Var Name

          | Concat Expr Expr

          | Compare Expr Expr

          | FunCall Name [Expr] -- Fun is function, Name is name of function, [Value] are arguments
          | InputExpr

  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
             | While Expr [Command]
             | If Expr [Command] [Command]
  deriving Show

data Value = IntVal Int | FltVal Float | StrVal String | BoolVal Bool | NullVal | Input
  deriving (Show, Eq)

data BTree = Leaf | Node (Name, Value) BTree BTree

instance Show BTree where
  show btree = show (inorderTraversal btree)

-- Inorder traversal of the binary tree, only used for instance of show.
inorderTraversal :: BTree -> [(Name, Value)]
inorderTraversal Leaf                             = []
inorderTraversal (Node (name, value) ltree rtree) = inorderTraversal ltree ++ [(name, value)] ++ inorderTraversal rtree

btreeLookup :: Name -> BTree -> Maybe Value
btreeLookup _name Leaf = Nothing
btreeLookup _name (Node (name, value) ltree rtree)
  | _name < name = btreeLookup _name ltree
  | _name > name = btreeLookup _name rtree 
  | otherwise    = Just value


eval :: BTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x)      = Just x -- for values, just give the value directly
eval vars (Var x)      = btreeLookup x vars -- using "lookup x (inorderTraversal vars)" here is against the purpose of using binary search tree.
eval vars (ToString x) = Just (StrVal (show x))
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Just (StrVal a), Just (StrVal b)) -> Just (StrVal (a ++ b))
  _                                  -> Nothing
eval vars (InputExpr)  = Just Input
eval vars expr = case (eval vars x, eval vars y) of
  (Just (FltVal a), Just (FltVal b)) -> Just (FltVal (func a b))
  (Just (FltVal f), Just (IntVal i)) -> Just (FltVal (func f (fromIntegral i)))
  (Just (IntVal i), Just (FltVal f)) -> Just (FltVal (func (fromIntegral i) f))
  (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (round (func (fromIntegral a) (fromIntegral b))))
  _                                  -> Nothing
  where
    (func, x, y) = case expr of
      Add expr1 expr2 -> ((+), expr1, expr2)
      Sub expr1 expr2 -> ((-), expr1, expr2)
      Mul expr1 expr2 -> ((*), expr1, expr2)
      Div expr1 expr2 -> ((/), expr1, expr2)

-- COMMAND AND EXPRESSION PARSER
-- pCommand :: Parser Command
-- pCommand = do t <- identifier
              -- symbol "="
              -- e <- pExpr
              -- return (Set t e)
            -- ||| do string "print"
                   -- space
                   -- e <- pExpr
                   -- return (Print e)
                 -- ||| do string "quit"
                        -- return Quit

pExpr :: Parser Expr
pExpr = (do symbol "input"
            return InputExpr)
        ||| (do t <- pTerm
                do symbol "+"
                   e <- pExpr
                   return (Add t e)
                 ||| do symbol "-"
                        e <- pExpr
                        return (Sub t e)
                      ||| return t)
        ||| (do s <- pStringExpr
                return s)

pFactor :: Parser Expr
pFactor = do f <- pFuncCall
             return f
          ||| do f <- float
                 return (Val (FltVal f))
              ||| do d <- integer
                     return (Val (IntVal d))
                  ||| do v <- identifier
                         return (Var v)
                      ||| do symbol "("
                             e <- pExpr
                             symbol ")"
                             return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| return f

-- STRING PARSER
pString :: Parser Expr
pString = do char '"'
             str <- many (sat (/= '"'))
             char '"'
             return (Val (StrVal str))

pStringExpr :: Parser Expr
pStringExpr = do s <- pString
                 do symbol "++"
                    s2 <- pStringExpr
                    return (Concat s s2)
                  ||| return s

-- STATEMENT PARSER
pStatement :: Parser Command
pStatement = (do s <- pIfStmt
                 return (s))
             ||| (do s <- pWhileStmt
                     return (s))
             ||| (do s <- pAssignmentStmt
                     return (s))
             ||| (do s <- pPrintStmt
                     return (s))
             ||| (do s <- pQuitStmt
                     return (s))

pStmtBlock :: Parser [Command]
pStmtBlock = do symbol "{"
                stmts <- many pStatement
                symbol "}"
                return stmts

pIfStmt :: Parser Command
pIfStmt = do string "if"
             space
             expression <- pBoolExpr
             string "then"
             space
             stmtBlock <- pStmtBlock
             string "else"
             eStmtBlock <- pStmtBlock
             return (If expression stmtBlock eStmtBlock)

pWhileStmt :: Parser Command
pWhileStmt = do string "while"
                space
                expression <- pBoolExpr
                space
                string "then"
                space
                stmtBlock <- pStmtBlock
                return (While expression stmtBlock)

pAssignmentStmt :: Parser Command
pAssignmentStmt = do t <- identifier
                     symbol "="
                     e <- pExpr
                     return (Set t e)

pPrintStmt :: Parser Command
pPrintStmt = do string "print"
                space
                e <- pExpr
                return (Print e)

pQuitStmt :: Parser Command
pQuitStmt = do string "quit"
               return Quit

-- FUNCTION PARSER 
pFuncName :: [(String, [Value])] -> Parser Expr
pFuncName [] = failure
pFuncName ((x, x2):xs) = do name <- symbol x
                            args <- pFuncArg x2
                            return (FunCall name args)
                            ||| pFuncName xs

pFuncArg :: [Value] -> Parser [Expr]
pFuncArg xs = do symbol "("
                 i <- (pArgs xs [])
                 symbol ")"
                 return (i)

pArgs :: [Value] -> [Expr] -> Parser [Expr]
pArgs (x :[]) ys | x == NullVal = return ys
                 | otherwise = do i <- pExpr
                                  return (i:ys)
pArgs (x :xs) ys | x == NullVal = pArgs xs ys
                 | otherwise    = do i <- pExpr
                                     symbol ","
                                     pArgs xs (i:ys)
pArgs _ _ = failure

pFuncCall :: Parser Expr
pFuncCall = do p <- pFuncName initFunc
               return (p)

data Compare = EQ | NE | GT | LT

pBoolExpr :: Parser Expr
pBoolExpr = (do symbol "("
                symbol "True"
                symbol ")"
                return (Val (BoolVal True)))
            ||| (do symbol "("
                    symbol "False"
                    symbol ")"
                    return (Val (BoolVal False)))

initFunc :: [(String, [Value])]
initFunc = [("input", [NullVal]), ("abs", [IntVal 0]), ("mod", [IntVal 0]), ("power", [IntVal 0])]

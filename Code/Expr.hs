module Expr where

import Parsing

import Data.Maybe

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | ToString Expr
          | Val Value
          | Var Name
          | Concat Expr Expr
  deriving Show

-- data StrExpr = V

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
  deriving Show

data Value = IntVal Int | FltVal Float | StrVal String
  deriving Show

data BTree = Leaf | Node (Name, Value) BTree BTree

instance Show BTree where
  show btree = show (inorderTraversal btree)

-- Inorder traversal of the binary tree, only used for instance of show.
inorderTraversal :: BTree -> [(Name, Value)]
inorderTraversal Leaf = []
inorderTraversal (Node (name, value) ltree rtree) = inorderTraversal ltree ++ [(name, value)] ++ inorderTraversal rtree

btreeLookup :: Name -> BTree -> Maybe Value
btreeLookup _name Leaf = Nothing
btreeLookup _name (Node (name, value) ltree rtree)
  | _name < name = btreeLookup _name ltree
  | _name > name = btreeLookup _name rtree 
  | otherwise = Just value

eval :: BTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Var x) = btreeLookup x vars -- using "lookup x (inorderTraversal vars)" here is against the purpose of using binary search tree.
eval vars (ToString x) = Just (StrVal (show x))
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Just (StrVal a), Just (StrVal b)) -> Just (StrVal (a ++ b))
  _ -> Nothing
eval vars expr = case (eval vars x, eval vars y) of
  (Just (FltVal f1), Just (FltVal f2)) -> Just (FltVal (func f1 f2))
  (Just (FltVal f), Just (IntVal i)) -> Just (FltVal (func f (fromIntegral i)))
  (Just (IntVal i), Just (FltVal f)) -> Just (FltVal (func (fromIntegral i) f))
  (Just (IntVal i1), Just (IntVal i2)) -> Just (IntVal (round (func (fromIntegral i1) (fromIntegral i2))))
  _ -> Nothing
  where
    (func, x, y) = case expr of
      Add expr1 expr2 -> ((+), expr1, expr2)
      Sub expr1 expr2 -> ((-), expr1, expr2)
      Mul expr1 expr2 -> ((*), expr1, expr2)
      Div expr1 expr2 -> ((/), expr1, expr2)


digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- identifier
              symbol "="
              e <- pExpr
              return (Set t e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
                 ||| do string "quit"
                        return Quit

pExpr :: Parser Expr
pExpr = (do t <- pTerm
            do symbol "+"
               e <- pExpr
               return (Add t e)
             ||| do symbol "-"
                    e <- pExpr
                    return (Sub t e)
                  ||| return t)
        ||| do s <- pStringExpr
               return s

pFactor :: Parser Expr
pFactor = do f <- float
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

-- What happens when it doesnt find "
-- Maybe it looks till the end of file
-- and then results in an error?
-- Or can add a functionality for one line strings
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

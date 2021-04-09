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
          | Val Int
          | Var Name
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
  deriving Show

data Value = IntVal Int | StrVal String
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x)                                  = Just x -- for values, just give the value directly
eval vars (Add x y) | isNothing x' || isNothing y' = Nothing
                    | otherwise                    = Just ((fromJust x') + (fromJust y'))
                    where x' = eval vars x
                          y' = eval vars y
eval vars (Sub x y) | isNothing x' || isNothing y' = Nothing
                    | otherwise                    = Just ((fromJust x') - (fromJust y'))
                    where x' = eval vars x
                          y' = eval vars y
eval vars (Mul x y) | isNothing x' || isNothing y' = Nothing
                    | otherwise                    = Just ((fromJust x') * (fromJust y'))
                    where x' = eval vars x
                          y' = eval vars y
eval vars (Div x y) | isNothing x' || isNothing y' = Nothing
                    | otherwise                    = Just ((fromJust x') `div` (fromJust y'))
                    where x' = eval vars x
                          y' = eval vars y
eval vars (ToString x)                             = Nothing
eval vars (Var x)                                  = lookup x vars 

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
pExpr = do t <- pTerm
           do symbol "+"
              e <- pExpr
              return (Add t e)
            ||| do symbol "-"
                   e <- pExpr
                   return (Sub t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- integer
             return (Val d)
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


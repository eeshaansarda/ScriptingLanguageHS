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
          -- | Value
          | Var Name
          | Val' String
          | Concat Expr Expr
  deriving Show

-- data StrExpr = V

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
  deriving Show

data Value = IntVal Int | StrVal String
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x)                                                    = Just (IntVal x) -- for values, just give the value directly
eval vars (Add x y) | isNothing (getVal x') || isNothing (getVal y') = Nothing
                    | otherwise                                      = Just (IntVal (a + b))
                    where x'         = eval vars x
                          y'         = eval vars y
                          getVal val = case val of Just (IntVal i) -> Just i
                                                   _               -> Nothing
                          a          = fromJust (getVal x')
                          b          = fromJust (getVal y')
eval vars (Sub x y) | isNothing (getVal x') || isNothing (getVal y') = Nothing
                    | otherwise                                      = Just (IntVal (a - b))
                    where x'         = eval vars x
                          y'         = eval vars y
                          getVal val = case val of Just (IntVal i) -> Just i
                                                   _               -> Nothing
                          a          = fromJust (getVal x')
                          b          = fromJust (getVal y')
eval vars (Mul x y) | isNothing (getVal x') || isNothing (getVal y') = Nothing
                    | otherwise                                      = Just (IntVal (a * b))
                    where x'         = eval vars x
                          y'         = eval vars y
                          getVal val = case val of Just (IntVal i) -> Just i
                                                   _               -> Nothing
                          a          = fromJust (getVal x')
                          b          = fromJust (getVal y')
eval vars (Div x y) | isNothing (getVal x') || isNothing (getVal y') = Nothing
                    | otherwise                                      = Just (IntVal (a `div` b))
                    where x'         = eval vars x
                          y'         = eval vars y
                          getVal val = case val of Just (IntVal i) -> Just i
                                                   _               -> Nothing
                          a          = fromJust (getVal x')
                          b          = fromJust (getVal y')
eval vars (Concat x y) | isNothing (getVal x') || isNothing (getVal y') = Nothing
                       | otherwise                                      = Just (StrVal (a ++ b))
                       where x'         = eval vars x
                             y'         = eval vars y
                             getVal val = case val of Just (StrVal i) -> Just i
                                                      _               -> Nothing
                             a          = fromJust (getVal x')
                             b          = fromJust (getVal y')
eval vars (ToString x)                                               = Just (StrVal (show x))
eval vars (Var x)                                                    = lookup x vars

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

-- What happens when it doesnt find "
-- Maybe it looks till the end of file
-- and then results in an error?
-- Or can add a functionality for one line strings
pString :: Parser Expr
pString = do char '"'
             str <- many (sat (\x -> x /= '"'))
             char '"'
             return (Val' str)

pStringExpr :: Parser Expr
pStringExpr = do s <- pString
                 do symbol "++"
                    s2 <- pStringExpr
                    return (Concat s s2)
                  ||| return s

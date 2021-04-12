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

          | Compare Expr Expr

          -- This is function call
          | FunCall Name [Value] -- Fun is function, Name is name of function, [Value] are arguments
          -- Maybe Value should be Expression here
  deriving Show

-- data StrExpr = V

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
  deriving Show

data Type = IntVal | FltVal | StrVal | BoolVal | Null
data Value = IntVal Int | FltVal Float | StrVal String | BoolVal Bool | Null
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Var x) = lookup x vars
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
pFactor = do f <- pFuncCall initFunc
             return FunCall f
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

-- Statement -> whileStmt | ifStmt | assignmentStmt | printStmt | quitStmt
-- Statement -> whileStmt | ifStmt | assignmentStmt | functionCallStmt

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

pIfStmt :: Parser Command
pIfStmt = do string "if"
             space
             expression <- pBoolExpr
             string "then"
             space
             statement <- pStatement
             do string "else"
                eStatement <- pStatement
                return (If expression statement eStatement)
              ||| return (If expression statement Nothing)

pWhileStmt :: Parser Command
pWhileStmt = do string "while"
                space
                expression <- pBoolExpr
                space
                string "then"
                space
                statement <- pStatement
                return (While expression statement)

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

-- pBoolExpr :: Parser Compare
-- pBoolExpr = do

pFuncName :: [(String, [Type])] -> Parser Expression
pFuncName [] = failure
pFuncName ((x, x2):xs) = do case (symbol x) of
                              [] -> pFuncCall xs
                              [(v, out)] -> (do args <- pFuncArg x2
                                                return (Fun v args))
                                            ||| failure

-- Parser [Expression]?
pFuncArg :: [Type] -> Parser [Expression]
pFuncArg xs = do symbol "("
                 i <- pArgs xs
                 symbol ")"
                 return (i)
                 -- variable lookup

-- When evaluating, eval Expression then call function
pArgs :: [Type] -> Parser [Expression]
pArgs (x :[]) ys | x == Null = return ys
                 | otherwise = do i <- pExpr
                                  return (ys:i)
pArgs (x :xs) ys = do i <- pExpr
                      symbol ","
                      pArgs xs ys

pFuncCall :: Parser Expression
pFuncCall = do p <- pFuncName initFunc
               return (p)

data Compare = EQ | NE | GT | LT

-- data LibFunc = Input | Abs Integer | Abs Float |
               -- Mod Integer | Mod Float |
               -- Power Integer | Power Float

-- Function Overloading is going to have to wait
initFunc :: [(String, [Type])]
initFunc = [("input", [Null]), ("abs", [IntVal]), ("mod", [IntVal]), ("power", [IntVal])]

-- A data decl for "library functions"
  -- On second thought that would be a constraint and not functions could be added after
-- an array for all functions (including library and user defined)

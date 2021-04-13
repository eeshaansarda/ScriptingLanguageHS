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
          | FunCall Name [Expr] -- Fun is function, Name is name of function, [Value] are arguments
          | InputExpr

          -- Maybe Value should be Expr here
  deriving Show

-- data StrExpr = V

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
             | While Expr Command
             | If Expr Command Command
  deriving Show

-- data Value = IntVal | FltVal | StrVal | BoolVal | NullVal
data Value = IntVal Int | FltVal Float | StrVal String | BoolVal Bool | NullVal | Input
  deriving (Show, Eq)

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expr to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Var x) = lookup x vars
eval vars (ToString x) = Just (StrVal (show x))
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Just (StrVal a), Just (StrVal b)) -> Just (StrVal (a ++ b))
  _ -> Nothing
eval vars (InputExpr) = Just Input
eval vars (FunCall name args) = case name of
  "toString" -> toString args
  -- need to eval the expressions
    where toString :: [Expr] -> Maybe Value
          toString ((Val (IntVal i)):[])  = Just (StrVal (show i))
          -- toString ((FltVal i):[])  = Just (StrVal (show i))
          -- toString ((BoolVal i):[]) = Just (StrVal (show i))
          toString _                = Nothing
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
pFactor = do f <- pFunCall
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
             string "else"
             eStatement <- pStatement
             return (If expression statement eStatement)
             -- do string "else"
                -- eStatement <- pStatement
                -- return (If expression statement eStatement)
              -- ||| return (If expression statement Nothing)

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

pFuncName :: [(String, [Value])] -> Parser Expr
pFuncName [] = failure
pFuncName ((x, x2):xs) = do name <- symbol x
                            args <- pFuncArg x2
                            return (FunCall name args)
                            ||| pFuncName xs
-- pFuncName ((x, x2):xs) = do case (symbol x) of
                              -- [] -> (pFuncName xs)
                              -- [(name, out)] -> (do args <- pFuncArg x2
                                                   -- return (FunCall name args))
                                               -- ||| failure

-- Parser [Expr]?
pFuncArg :: [Value] -> Parser [Expr]
pFuncArg xs = do symbol "("
                 i <- (pArgs xs [])
                 symbol ")"
                 return (i)
                 -- variable lookup

-- When evaluating, eval Expr then call function
  -- Expressions should work, but matching them here is not possible
  -- Maybe eval it to get a [Value]
  -- Cannot use eval here because the loop's variables cannot be accessed here
pArgs :: [Value] -> [Expr] -> Parser [Expr]
pArgs (x :[]) ys | x == NullVal = return ys
                 | otherwise = do i <- pExpr
                                  return (i:ys)
pArgs (x :xs) ys | x == NullVal = pArgs xs ys
                 | otherwise    = do i <- pExpr
                                     symbol ","
                                     pArgs xs (i:ys)

pFunCall :: Parser Expr
pFunCall = do p <- pFuncName initFunc
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


-- data LibFunc = Input | Abs Integer | Abs Float |
               -- Mod Integer | Mod Float |
               -- Power Integer | Power Float

-- Function Overloading is going to have to wait
-- [(Function name, Arguments, Return Value?)] -> need to chanf
initFunc :: [(String, [Value])]
initFunc = [("input", [NullVal]), ("abs", [IntVal 0]), ("mod", [IntVal 0]), ("power", [IntVal 0]), ("toString", [IntVal 0])]

-- A data decl for "library functions"
  -- On second thought that would be a constraint and functions will not be able to be added after
-- an array for all functions (including library and user defined)

module Expr where

import Parsing

type Name = String

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr

          | Val Value
          | Var Name

          | Concat Expr Expr

          | Compare Expr Expr

          | FunCallExpr Name [Expr] -- Fun is function, Name is name of function, [Value] are arguments
          | InputExpr

          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Eq Expr Expr
          | Ne Expr Expr
          | Gt Expr Expr
          | Lt Expr Expr
          | Gte Expr Expr
          | Lte Expr Expr
  deriving (Show, Eq)

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit          -- quit the program
             | While Expr [Command]
             | If Expr [Command] [Command]
             | Import FilePath
             | Fun Name [Name] [Command] -- Name -> name of function, [Name] -> Argument variables, [Command] -> Commands in the function
             | VoidFunCall Name [Expr]
             | Return Expr
  deriving Show

data Value = IntVal Int | FltVal Float | StrVal String | BoolVal Bool | NullVal | Input | FunCall Name [Expr]
  deriving Eq

instance Show Value where
  show (IntVal i)  = show i
  show (FltVal f)  = show f
  show (StrVal s)  = show s
  show (BoolVal b) = show b
  show NullVal     = "NULL"
  show Input       = "INPUT"

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


eval :: BTree ->    -- Variable name to value mapping
        Expr ->     -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)

eval vars (Val x)             = Just x             -- for values, just give the value directly
eval vars (Var x)             = btreeLookup x vars -- using "lookup x (inorderTraversal vars)" here is against the purpose of using binary search tree.
eval vars (Concat x y)        = case (eval vars x, eval vars y) of
                                     (Just (StrVal a), Just (StrVal b)) -> Just (StrVal (a ++ b))
                                     _                                  -> Nothing
eval vars (InputExpr)         = Just Input
eval vars (FunCallExpr name args) = case name of
                                     "toString" -> toString args
                                       where toString :: [Expr] -> Maybe Value
                                             toString (intExpression:[])  = case eval vars intExpression of
                                               Just (IntVal i) -> (Just (StrVal (show i)))
                                               Just (FltVal f) -> (Just (StrVal (show f)))
                                               _               -> Nothing
                                     "toInt"    -> toInt args
                                       where toInt :: [Expr] -> Maybe Value
                                             toInt ((Val (StrVal i)):[])  = Just (IntVal (read i))
                                             toInt _                      = Nothing
                                     "toFloat"  -> toFlt args
                                       where toFlt :: [Expr] -> Maybe Value
                                             toFlt ((Val (StrVal i)):[])  = Just (FltVal (read i))
                                             toFlt _                      = Nothing
                                     _          -> Just (FunCall name args)
eval vars (Abs x)             = case eval vars x of
                                     Just (IntVal i) -> Just (IntVal (abs i))
                                     Just (FltVal f) -> Just (FltVal (abs f))
                                     _               -> Nothing
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (mod a b))
                                     _                                  -> Nothing
eval vars expr = case expr of
  Add e e2 -> floatOperations vars expr
  Sub e e2 -> floatOperations vars expr
  Mul e e2 -> floatOperations vars expr
  Div e e2 -> floatOperations vars expr
  Pow e e2 -> floatOperations vars expr
  Lt  e e2 -> boolOperations  vars expr
  Gt  e e2 -> boolOperations  vars expr
  Lte e e2 -> boolOperations  vars expr
  Gte e e2 -> boolOperations  vars expr
  Eq  e e2 -> boolOperations  vars expr
  Ne  e e2 -> boolOperations  vars expr
  Not e    -> notBoolOp       vars expr
  _        -> Nothing

floatOperations :: BTree -> Expr -> Maybe Value
floatOperations vars expr = case (eval vars x, eval vars y) of
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
      Pow expr1 expr2 -> ((**), expr1, expr2)

boolOperations :: BTree -> Expr -> Maybe Value
boolOperations vars expr = case (eval vars x, eval vars y) of
  (Just (StrVal  a), Just (StrVal  b)) -> Just (BoolVal (func a b))
  (Just (FltVal  a), Just (FltVal  b)) -> Just (BoolVal (func (show a) (show b)))
  (Just (IntVal  a), Just (IntVal  b)) -> Just (BoolVal (func (show a) (show b)))
  (Just (BoolVal a), Just (BoolVal b)) -> Just (BoolVal (func (show a) (show b)))
  _                                    -> Nothing
  --Just (BoolVal (func (eval vars x) (eval vars y)))
  where
    (func, x, y) = case expr of
      Lt  expr1 expr2 -> ((<),  expr1, expr2)
      Gt  expr1 expr2 -> ((>),  expr1, expr2)
      Lte expr1 expr2 -> ((<=), expr1, expr2)
      Gte expr1 expr2 -> ((>=), expr1, expr2)
      Eq  expr1 expr2 -> ((==), expr1, expr2)
      Ne  expr1 expr2 -> ((/=), expr1, expr2)

notBoolOp :: BTree -> Expr -> Maybe Value
notBoolOp vars (Not x) = case eval vars x of
  Just (BoolVal  a) -> Just (BoolVal (not a))
  _ -> Nothing
notBoolOp _ _ = Nothing

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
                     ||| do symbol "++"
                            e <- pExpr
                            return (Concat t e)
                          ||| return t)

pFactor :: Parser Expr
pFactor = do f <- pFunCall
             return f
          ||| do f <- float
                 return (Val (FltVal f))
              ||| do d <- integer
                     return (Val (IntVal d))
                  ||| do v <- identifier
                         return (Var v)
                      ||| do a <- pAbs
                             return a
                          ||| do symbol "("
                                 e <- pExpr
                                 symbol ")"
                                 return e
                               ||| do s <- pString
                                      return s

pTerm :: Parser Expr
pTerm = do f <- pPower
           do symbol "*"
              t <- pTerm
              return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
                 ||| do symbol "%"
                        t <- pTerm
                        return (Mod f t)
                      ||| return f

pAbs :: Parser Expr
pAbs = do symbol "|"
          e <- pExpr
          symbol "|"
          return (Abs e)

pPower :: Parser Expr
pPower = do f <- pFactor
            do symbol "^"
               p <- pPower
               return (Pow f p)
             ||| return f

-- STRING PARSER
pString :: Parser Expr
pString = do ch <- char '"' ||| char '\''
             str <- many (sat (/= ch))
             char ch
             return (Val (StrVal str))


-- Statement -> whileStmt | ifStmt | assignmentStmt | printStmt | quitStmt
-- Statement -> whileStmt | ifStmt | assignmentStmt | functionCallStmt



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
             ||| (do s <- pImportStmt
                     return (s))
             ||| (do s <- pFun
                     return (s))
             ||| (do s <- pVoidFunCall
                     return (s))
             ||| (do s <- pReturnStmt
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
             stmtBlock <- pStmtBlock
             string "else"
             eStmtBlock <- pStmtBlock
             return (If expression stmtBlock eStmtBlock)

pWhileStmt :: Parser Command
pWhileStmt = do string "while"
                space
                expression <- pBoolExpr
                space
                stmtBlock <- pStmtBlock
                return (While expression stmtBlock)

pAssignmentStmt :: Parser Command
pAssignmentStmt = do t <- identifier
                     symbol "="
                     (do e <- pExpr
                         return (Set t e)
                       ||| do e <- pBoolExpr
                              return (Set t e))

pPrintStmt :: Parser Command
pPrintStmt = do string "print"
                space
                (do e <- pExpr
                    return (Print e)
                 ||| do e <- pBoolExpr
                        return (Print e))

pQuitStmt :: Parser Command
pQuitStmt = do string "quit"
               return Quit

pImportStmt :: Parser Command
pImportStmt = do string "import"
                 space
                 ch <- char '"' ||| char '\''
                 filepath <- many (sat (/= ch))
                 char ch
                 return (Import filepath)

pReturnStmt :: Parser Command
pReturnStmt = do string "return"
                 space
                 e <- pExpr
                 return (Return e)

-- FUNCTION PARSER 
pFunCall :: Parser Expr
pFunCall = do name <- identifier
              args <- pFunCallArgs
              return (FunCallExpr name args)

pVoidFunCall :: Parser Command
pVoidFunCall = do name <- identifier
                  args <- pFunCallArgs
                  return (VoidFunCall name args)

pFunCallArgs :: Parser [Expr]
pFunCallArgs = do symbol "("
                  i <- (pCSExpressions [])
                  return (i)

-- Comma seperated expressions
pCSExpressions :: [Expr] -> Parser [Expr]
pCSExpressions [] = (do symbol ")"
                        return [])
                       ||| (do i <- pExpr
                               pCSExpressions (i:[]))
pCSExpressions ys = (do symbol ","
                        i <- pExpr
                        pCSExpressions (i:ys))
                       ||| (do symbol ")"
                               return (reverse ys))


-- For function calls, store the previous state, execute the function, restore the state, and then update the state (with the function results)
pFun :: Parser Command
pFun = do string "fun"
          name <- identifier
          symbol "("
          vars <- pCSVar [] -- This absorbs the ")"
          commands <- pStmtBlock -- last statement be return?
          return (Fun name vars commands)

pCSVar :: [Name] -> Parser [Name]
pCSVar [] = (do symbol ")"
                return [])
               ||| (do i <- identifier
                       pCSVar (i:[]))
pCSVar ys = (do symbol ","
                i <- identifier
                pCSVar (i:ys))
               ||| (do symbol ")"
                       return (reverse ys))
-- pCSVar :: [Name] -> Parser [Name]
-- pCSVar [] = do i <- identifier
               -- return (i:[])
-- pCSVar ys = do symbol ","
               -- i <- identifier
               -- pCSVar (i:ys)

-- TODO Sorry for the bad naming choice
-- please search and replace if possible
pBoolFactor :: Parser Expr
pBoolFactor = (do e <- pExpr
                  symbol "<"
                  e2 <- pExpr
                  return (Lt e e2))
              ||| (do e <- pExpr
                      symbol ">"
                      e2 <- pExpr
                      return (Gt e e2))
              ||| (do e <- pExpr
                      symbol "=="
                      e2 <- pExpr
                      return (Eq e e2))
              ||| (do e <- pExpr
                      symbol "!="
                      e2 <- pExpr
                      return (Ne e e2))
              ||| (do e <- pExpr
                      symbol ">="
                      e2 <- pExpr
                      return (Gte e e2))
              ||| (do e <- pExpr
                      symbol "<="
                      e2 <- pExpr
                      return (Lte e e2))

pBoolFact :: Parser Expr
pBoolFact = (do symbol "True"
                return (Val (BoolVal True))
            ||| do symbol "False"
                   return (Val (BoolVal False)))
                ||| (do f <- pBoolFactor
                        return f)
                ||| (do symbol "!"
                        f <- pBoolFact
                        return (Not f))
                ||| (do symbol "("
                        f <- pBoolFact
                        symbol ")"
                        return f)

pBoolTerm :: Parser Expr
pBoolTerm = do f <- pBoolFact
               (do symbol "&&"
                   f2 <- pBoolFact
                   return (And f f2))
                 ||| return f

pBoolExpr :: Parser Expr
pBoolExpr = do f <- pBoolTerm
               (do symbol "||"
                   f2 <- pBoolTerm
                   return (Or f f2))
                 ||| return f

-- A data decl for "library functions"
  -- On second thought that would be a constraint and functions will not be able to be added after
-- an array for all functions (including library and user defined)

module LangParser where

import Parsing
import Expr

-- COMMAND AND EXPRESSION PARSER

-- Statement -> whileStmt | ifStmt | assignmentStmt | printStmt | quitStmt | functionCallStmt

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
             ||| (do s <- pExpr_
                     return (s))

-- Block of statements (if while functions)
pStmtBlock :: Parser [Command]
pStmtBlock = do symbol "{"
                stmts <- many pStatement
                symbol "}"
                return stmts

-- If statements
pIfStmt :: Parser Command
pIfStmt = do string "if"
             space
             expression <- pBoolExpr
             stmtBlock <- pStmtBlock
             string "else"
             eStmtBlock <- pStmtBlock
             return (If expression stmtBlock eStmtBlock)

-- While statements
pWhileStmt :: Parser Command
pWhileStmt = do string "while"
                space
                expression <- pBoolExpr
                space
                stmtBlock <- pStmtBlock
                return (While expression stmtBlock)

-- Assignment statements
pAssignmentStmt :: Parser Command
pAssignmentStmt = do t <- identifier
                     symbol "="
                     (do e <- pExpr
                         return (Set t e)
                       ||| do e <- pBoolExpr
                              return (Set t e))

-- Print statements
pPrintStmt :: Parser Command
pPrintStmt = do string "print"
                space
                (do e <- pExpr
                    return (Print e)
                 ||| do e <- pBoolExpr
                        return (Print e))

-- Quit statements
pQuitStmt :: Parser Command
pQuitStmt = do string "quit"
               return Quit

-- Import statements
pImportStmt :: Parser Command
pImportStmt = do string "import"
                 space
                 ch <- char '"' ||| char '\''
                 filepath <- many (sat (/= ch))
                 char ch
                 return (Import filepath)

-- Return statements
pReturnStmt :: Parser Command
pReturnStmt = do string "return"
                 space
                 e <- pExpr
                 return (Return e)

-- FUNCTION PARSER
-- Function Call statement
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


-- Function definition statement
pFun :: Parser Command
pFun = do string "fun"
          name <- identifier
          symbol "("
          vars <- pCSVar []      -- This absorbs the ")"
          commands <- pStmtBlock
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

-- For expressions to be printed
pExpr_ :: Parser Command
pExpr_ = (do Expr <$> pBoolExpr) ||| (do Expr <$> pExpr)

-- EXPRESSION Parsers
-- Numeric/String expressions
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


-- Boolean Expressions
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
              ||| (do e <- identifier
                      return (Var e))

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
                        f <- pBoolExpr
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

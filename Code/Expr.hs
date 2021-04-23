module Expr where

import Data.Either

type Name = String
type ErrMsg = String
type ExprName = String

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
             | Expr Expr
  deriving Show

data EvalError = ExprErr ExprName ErrMsg

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

btreeLookup :: Name -> BTree -> Either EvalError Value
btreeLookup _name Leaf = Left (ExprErr "Var" (_name ++ " not found"))
btreeLookup _name (Node (name, value) ltree rtree)
  | _name < name = btreeLookup _name ltree
  | _name > name = btreeLookup _name rtree 
  | otherwise    = Right value


eval :: BTree -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either EvalError Value -- Result (if no errors such as missing variables)
eval vars (Val x)      = Right x -- for values, just give the value directly
eval vars (Var x)      = btreeLookup x vars -- using "lookup x (inorderTraversal vars)" here is against the purpose of using binary search tree.
eval vars (Concat x y) = case (eval vars x, eval vars y) of
  (Right (StrVal a), Right (StrVal b)) -> Right (StrVal (a ++ b))
  (Right (StrVal a), Right not_string) -> Left (ExprErr "Concat" (show not_string ++ " is not a string"))
  (Right (StrVal a), Left eval_err)    -> Left eval_err
  (Right not_string, _)                -> Left (ExprErr "Concat" (show not_string ++ " is not a string"))
  (Left eval_err, _)                   -> Left eval_err
eval vars InputExpr         = Right Input
eval vars (FunCallExpr name args) = case name of
                                     "toString" -> toString args
                                       where toString :: [Expr] -> Either EvalError Value
                                             toString (intExpression:[])  = case eval vars intExpression of
                                               Right (IntVal i) -> (Right (StrVal (show i)))
                                               Right (FltVal f) -> (Right (StrVal (show f)))
                                               Right (StrVal s) -> (Right (StrVal s))
                                               _               -> Left (ExprErr "toString" (show intExpression ++ " cannot be converted to string"))
                                     "toInt"    -> toInt args

                                       where toInt :: [Expr] -> Either EvalError Value
                                             toInt ((Val (StrVal i)):[])  = Right (IntVal (read i :: Int))
                                             toInt arg                      = Left (ExprErr "toInt" (show args ++ " cannot be converted to integer"))
                                     "toFloat"  -> toFlt args
                                       where toFlt :: [Expr] -> Either EvalError Value
                                             toFlt ((Val (StrVal i)):[])  = Right (FltVal (read i :: Float))
                                             toFlt arg                      = Left (ExprErr "toFlt" (show args ++ " cannot be converted to float"))
                                     _          -> Right (FunCall name args)

eval vars (Abs x)             = case eval vars x of
                                     Right (IntVal i) -> Right (IntVal (abs i))
                                     Right (FltVal f) -> Right (FltVal (abs f))
                                     _               -> Left (ExprErr "Abs" (show x ++ " is not a float or an integer"))
eval vars (Mod x y)           = case (eval vars x, eval vars y) of
                                     (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (mod a b))
                                     (Right (IntVal a), Right not_int) -> Left (ExprErr "Mod" (show not_int ++ " is not an integer"))
                                     (Right (IntVal a), Left eval_err) -> Left eval_err
                                     (Right not_int, _) -> Left (ExprErr "Mod" (show not_int ++ " is not an integer"))
                                     (Left eval_err, _) -> Left eval_err
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
  And e e2 -> andorBoolOp     vars expr
  Or  e e2 -> andorBoolOp     vars expr
  Not e    -> notBoolOp       vars expr
  op        -> Left (ExprErr (show op) ("Unknown operations: " ++ show op))

floatOperations :: BTree -> Expr -> Either EvalError Value
floatOperations vars expr = case (eval vars x, eval vars y) of
  (Right (FltVal a), Right (FltVal b)) -> Right (FltVal (func a b))
  (Right (FltVal f), Right (IntVal i)) -> Right (FltVal (func f (fromIntegral i)))
  (Right (IntVal i), Right (FltVal f)) -> Right (FltVal (func (fromIntegral i) f))
  (Right (IntVal a), Right (IntVal b)) -> Right (IntVal (round (func (fromIntegral a) (fromIntegral b))))
  (Right (FltVal f), Right not_num)    -> Left (ExprErr "Add | Sub | Mul | Div" (show not_num ++ " is not a number"))
  (Right (IntVal i), Right not_num)    -> Left (ExprErr "Add | Sub | Mul | Div" (show not_num ++ " is not a number"))
  (Right (FltVal f), Left eval_err)    -> Left eval_err
  (Right (IntVal f), Left eval_err)    -> Left eval_err
  (Right not_num, _)                   -> Left (ExprErr "Add | Sub | Mul | Div" (show not_num ++ " is not a number"))
  (Left eval_err, _)                     -> Left eval_err
  where
    (func, x, y) = case expr of
      Add expr1 expr2 -> ((+), expr1, expr2)
      Sub expr1 expr2 -> ((-), expr1, expr2)
      Mul expr1 expr2 -> ((*), expr1, expr2)
      Div expr1 expr2 -> ((/), expr1, expr2)
      Pow expr1 expr2 -> ((**), expr1, expr2)


boolHelper :: Ord a => Expr -> a -> a -> Bool
boolHelper (Lt expr1 expr2) = (<)
boolHelper (Gt expr1 expr2) = (>)
boolHelper (Lte expr1 expr2) = (<=)
boolHelper (Gte expr1 expr2) = (>=)
boolHelper (Eq expr1 expr2) = (==)
boolHelper (Ne expr1 expr2) = (/=)

boolOperations :: BTree -> Expr -> Either EvalError Value
boolOperations vars expr = case (eval vars x, eval vars y) of
  (Right (StrVal  a), Right (StrVal  b)) -> Right (BoolVal (elem (compare a b) ordering))
  (Right (FltVal  a), Right (FltVal  b)) -> Right (BoolVal (elem (compare a b) ordering))
  (Right (FltVal  a), Right (IntVal  b)) -> Right (BoolVal (elem (compare a (fromIntegral b)) ordering))
  (Right (IntVal  a), Right (FltVal  b)) -> Right (BoolVal (elem (compare (fromIntegral a) b) ordering))
  (Right (IntVal  a), Right (IntVal  b)) -> Right (BoolVal (elem (compare a b) ordering))
  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (elem (compare a b) ordering))
  (Right a, Right b) -> Left (ExprErr "boolOperations" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
  (Right _, Left eval_err) -> Left eval_err
  (Left eval_err, _) -> Left eval_err
  where
    (ordering, x, y) = case expr of
      Lt  expr1 expr2 -> ([LT],  expr1, expr2)
      Gt  expr1 expr2 -> ([GT],  expr1, expr2)
      Lte expr1 expr2 -> ([LT, EQ], expr1, expr2)
      Gte expr1 expr2 -> ([GT, EQ], expr1, expr2)
      Eq  expr1 expr2 -> ([EQ], expr1, expr2)
      Ne  expr1 expr2 -> ([LT, GT], expr1, expr2)

notBoolOp :: BTree -> Expr -> Either EvalError Value
notBoolOp vars (Not x) = case eval vars x of
  Right (BoolVal  a) -> Right (BoolVal (not a))
  Right not_bool -> Left (ExprErr "not" (show not_bool ++ " is not a boolean"))
  Left eval_err -> Left eval_err

andorBoolOp :: BTree -> Expr -> Either EvalError Value
andorBoolOp vars expr = case (eval vars x, eval vars y) of
  (Right (BoolVal a), Right (BoolVal b)) -> Right (BoolVal (func a b) )
  (Right a, Right b) -> Left (ExprErr "andorBoolOp" ("Bool operations between " ++ show x ++ " and " ++ show y ++ " are not supported"))
  (Right _, Left eval_err) -> Left eval_err
  (Left eval_err, _) -> Left eval_err
  where
    (func, x, y) = case expr of
      And expr1 expr2 -> ((&&), expr1, expr2)
      Or  expr1 expr2 -> ((||), expr1, expr2)


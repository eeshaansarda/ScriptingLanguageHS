{-# LANGUAGE TemplateHaskell #-}
module QuickCheckTests where

import REPL
import Expr
import Parsing
import LangParser

import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.All

-- Type generators 
-- https://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings
data Operator = Operator String 
  deriving Show

instance Arbitrary Operator where 
  arbitrary = do e <- elements "+-*/%^"
                 return (Operator $ filter (/='\'') $ show e)

-- http://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#g:8
data Space = Space String
  deriving Show

instance Arbitrary Space where
  arbitrary = do x <- listOf $ elements " "
                 return (Space x)


-- PARSER TESTS - checks that parser converts strings to appropriate type representation

-- Checks that ints are parsed correctly
prop_parseInt :: Int -> Bool
prop_parseInt int = case parse pExpr (show int) of
                         [(Val (IntVal a), "")] -> True
                         _                      -> False

-- Checks that strings are parsed correctly
prop_parseString :: String -> Bool
prop_parseString str = case parse pExpr ("\"" ++ str ++ "\"") of
                            [(Val (StrVal a), "")] -> True
                            _                      -> False

-- Checks that floats are parsed correctly
prop_parseFloat :: Float -> Bool
prop_parseFloat flt = case parse pExpr (show flt) of
                           [(Val (FltVal a), "")] -> True
                           _                      -> False

-- Checks that binary operators (+ - * / % ^) are correctly parsed for ints (ignoring whitespace)
prop_parseOperatorInt :: Int -> Int -> Space -> Operator -> Bool
prop_parseOperatorInt a b (Space s) (Operator o) = case parse pExpr ((show a) ++ s ++ o ++ s ++ (show b)) of
                                                        [(Add a b, "")] -> True
                                                        [(Sub a b, "")] -> True
                                                        [(Mul a b, "")] -> True
                                                        [(Div a b, "")] -> True
                                                        [(Mod a b, "")] -> True
                                                        [(Pow a b, "")] -> True
                                                        _               -> False

-- Checks that binary operators (+ - * / % ^) are correctly parsed for floats (ignoring whitespace)
prop_parseOperatorFlt :: Float -> Float -> Space -> Operator -> Bool
prop_parseOperatorFlt a b (Space s) (Operator o) = case parse pExpr ((show a) ++ s ++ o ++ s ++ (show b)) of
                                                        [(Add a b, "")] -> True
                                                        [(Sub a b, "")] -> True
                                                        [(Mul a b, "")] -> True
                                                        [(Div a b, "")] -> True
                                                        [(Mod a b, "")] -> True
                                                        [(Pow a b, "")] -> True
                                                        _               -> False

-- Checks that Abs gets correctly parsed for ints
prop_parseAbsInt :: Int -> Bool
prop_parseAbsInt int = case parse pExpr ("|" ++ show int ++ "|") of
                            [(Abs a, "")] -> True
                            _             -> False

-- Checks that Abs gets correctly parsed for floats
prop_parseAbsFlt :: Float -> Bool
prop_parseAbsFlt flt = case parse pExpr ("|" ++ show flt ++ "|") of
                            [(Abs a, "")] -> True
                            _             -> False

-- Checks that concatenate correctly parses
prop_parseConcat :: String -> String -> Bool
prop_parseConcat str1 str2 = case parse pExpr ("\"" ++ str1 ++ "\"" ++ "++" ++  "\"" ++ str2 ++ "\"") of
                                  [(Concat a b, "")] -> True
                                  _                  -> False


-- EVAL TESTS - checks that eval operates correctly

-- Checks that eval converts ints to floats for Add
prop_evalAddConvert :: Int -> Float -> Bool
prop_evalAddConvert i f = case eval Leaf (Add (Val (IntVal i)) (Val(FltVal f))) of 
                               Right (FltVal a) -> True 
                               _                -> False

-- Checks that eval converts ints to floats for Sub
prop_evalSubConvert :: Int -> Float -> Bool
prop_evalSubConvert i f = case eval Leaf (Sub (Val (IntVal i)) (Val(FltVal f))) of 
                               Right (FltVal a) -> True 
                               _                -> False

-- Checks that eval converts ints to floats for Mul
prop_evalMulConvert :: Int -> Float -> Bool
prop_evalMulConvert i f = case eval Leaf (Mul (Val (IntVal i)) (Val(FltVal f))) of 
                               Right (FltVal a) -> True 
                               _                -> False

-- Checks that eval converts int to floats for Div
prop_evalDivConvert :: Int -> Float -> Bool
prop_evalDivConvert i f = if f == 0.0 then True else -- Need a better way to filter out divide by zero
                          case eval Leaf (Div (Val (IntVal i)) (Val (FltVal f)) ) of 
                               Right (FltVal a) -> True 
                               _                -> False

return []
runTests = $quickCheckAll

main :: IO ()
main = do runTests
          return ()
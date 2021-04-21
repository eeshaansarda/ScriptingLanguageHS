{-# LANGUAGE TemplateHaskell #-}

import REPL
import Expr
import Parsing

import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.All

-- Type generators - https://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings
data Operator = Operator String 
  deriving Show

instance Arbitrary Operator where 
    arbitrary = oneof [return (Operator "+"),
                       return (Operator "-"),
                       return (Operator "*"),
                       return (Operator "/"),
                       return (Operator "%"),
                       return (Operator "^")]


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

-- Checks that binary operators (+ - * / % ^) are correctly parsed for ints
prop_parseOperator :: Int -> Int -> Operator -> Bool
prop_parseOperator a b (Operator o) = case parse pExpr ((show a) ++ o ++ (show b)) of
                                           [(Add a b, "")] -> True
                                           [(Sub a b, "")] -> True
                                           [(Mul a b, "")] -> True
                                           [(Div a b, "")] -> True
                                           [(Mod a b, "")] -> True
                                           [(Pow a b, "")] -> True
                                           _               -> False

-- Checks that binary operators (+ - * / % ^) are correctly parsed for floats
prop_parseOperator :: Float -> Float -> Operator -> Bool
prop_parseOperator a b (Operator o) = case parse pExpr ((show a) ++ o ++ (show b)) of
                                           [(Add a b, "")] -> True
                                           [(Sub a b, "")] -> True
                                           [(Mul a b, "")] -> True
                                           [(Div a b, "")] -> True
                                           [(Mod a b, "")] -> True
                                           [(Pow a b, "")] -> True
                                           _               -> False

-- Checks that Abs gets correctly parsed for ints
prop_parseAbs :: Int -> Bool
prop_parseAbs int = case parse pExpr ("|" ++ show int ++ "|") of -- TODO try and make valid arbitrary expressions to put in the middle?
                         [(Abs a, "")] -> True
                         _             -> False

-- Checks that Abs gets correctly parsed for floats
prop_parseAbs :: Float -> Bool
prop_parseAbs flt = case parse pExpr ("|" ++ show flt ++ "|") of -- TODO try and make valid arbitrary expressions to put in the middle?
                         [(Abs a, "")] -> True
                         _             -> False

return []
runTests = $quickCheckAll

main :: IO ()
main = do runTests
          return ()
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Name (VarName (..), showVarName, FunctionName (..), showFunctionName, parseVarName, parseFunctionName) where

import Text.Parsec (many)
import Text.Parsec.Char (alphaNum, char, digit, letter)
import Text.Parsec.Combinator (many1)
import Text.Parsec.String (Parser)
import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint, text)

newtype VarName = VarName {unVarName :: Int} deriving (Eq, Ord, Show, Num)
newtype FunctionName = FunctionName {unFunctionName :: String} deriving (Eq, Ord, Show)

instance Pretty VarName where
  pPrint = text . showVarName

instance Pretty FunctionName where
  pPrint = text . showFunctionName

showVarName :: VarName -> String
showVarName (VarName k) = "x" ++ show k

parseVarName :: Parser VarName
parseVarName = VarName . read <$> (char 'x' *> many1 digit)

showFunctionName :: FunctionName -> String
showFunctionName (FunctionName fn) = fn

parseFunctionName :: Parser FunctionName
parseFunctionName = (FunctionName .) . (:) <$> letter <*> many alphaNum

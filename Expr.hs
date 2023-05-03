module Expr (Atom (..), Expr (..), exprType, showExpr, parseExpr) where

import Atom
import Func
import Tensor
import Types

import Control.Applicative ((<|>))
import Text.Parsec (between, many1, option, sepBy, try)
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.String (Parser)
import qualified Text.PrettyPrint.HughesPJClass as PP

data Expr = Expr
    { exprTy :: Type
    , func :: Func
    , params :: [Atom]
    }
    deriving (Eq, Show)

instance PP.Pretty Expr where
    pPrint = PP.text . showExpr

exprType :: Expr -> Type
exprType (Expr t _ _) = t

showExpr :: Expr -> String
showExpr (Expr _ f as) = showFunc f as

parseExpr :: Parser Expr
parseExpr =
    try parseUnaryApplication
        <|> try parseBinaryApplication
        <|> try parseDot
        <|> try parseBroadcast
        <|> try parseReshape
        <|> try parseTranspose
        <|> parseConstant
  where
    parseUnaryApplication =
        Expr UnknownType . FUnaryPointwise
            <$> parseUnaryPointwiseOp
            <*> (spaces *> (pure <$> parseAtom))
    parseBinaryApplication =
        makeBinary
            <$> parseAtom
            <*> (parseBinaryPointwiseOp <* spaces)
            <*> parseAtom
    makeBinary arg1 op arg2 = Expr UnknownType (FBinaryPointwise op) [arg1, arg2]
    parseDot =
        (Expr UnknownType FDot .)
            <$> ((:) <$> (string "dot" *> spaces *> parseAtom))
            <*> (return <$> (spaces *> parseAtom))
    parseBroadcast =
        (Expr UnknownType .) . FBroadcast
            -- A set of indices isn't actually a shape, but if it floats like a shape and
            -- it quacks like a shape...
            <$> (string "broadcast" *> spaces *> parseShape <* spaces)
            <*> (parseShape <* spaces)
            <*> (pure <$> parseAtom)
    parseReshape =
        Expr UnknownType . FReshape
            <$> (string "reshape" *> spaces *> parseShape <* spaces)
            <*> (pure <$> parseAtom)
    -- A permutation isn't actually a shape, but if it floats like a shape and
    -- it quacks like a shape...
    parseTranspose =
        Expr UnknownType . FTranspose
            <$> (string "transpose" *> spaces *> parseShape <* spaces)
            <*> (pure <$> parseAtom)
    parseConstant = do
        ty <- string "constant" *> spaces *> parseType
        tensor <- parseTensor ty
        return (Expr ty (FConstant ty tensor) [])

parseTensor :: Type -> Parser Tensor
parseTensor (Type sh bt) | bt `elem` [TFloat32, TBFloat16] = do
    tensor <- mkFloatTensor bt sh <$> parseDecimalList
    case tensor of
        Right t -> return t
        Left e -> error (PP.prettyShow e)
parseTensor ty = error $ "Can't parse a constant of type " ++ showType ty

parseDecimalList :: Read a => Parser [a]
parseDecimalList = map read <$> bracketed parseDecimals
  where
    bracketed = between (spaces *> string "[" <* spaces) (string "]" <* spaces)
    parseDecimals = parseDecimal `sepBy` (string "," <* spaces)
    parseDecimal = (++) <$> digits <*> decimal <* spaces
    digits = many1 digit
    decimal = option "" $ (:) <$> char '.' <*> digits

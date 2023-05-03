{-# LANGUAGE LambdaCase #-}

module Func (
    Func (..),
    showFunc,
    UnaryPointwiseOp (..),
    showUnaryPointwiseOp,
    BinaryPointwiseOp (..),
    showBinaryPointwiseOp,
    parseUnaryPointwiseOp,
    parseBinaryPointwiseOp,
) where

import Atom
import Name
import Tensor
import Types

import Control.Applicative ((<|>))
import Text.Parsec (try)
import Text.Parsec.Char (string)
import Text.Parsec.String (Parser)

-- We spell out the identity as "unary copy".
data UnaryPointwiseOp = Copy | Sin | Cos | Exp | Negate deriving (Eq, Ord, Show)
data BinaryPointwiseOp = Add | Sub | Mul | Div deriving (Eq, Ord, Show)
data Func
    = FUnaryPointwise UnaryPointwiseOp
    | FBinaryPointwise BinaryPointwiseOp
    -- Ordinary rank-2 matrix product.
    | FDot
    -- The semantics of the [Int] are as in XLA's BroadcastInDim.
    | FBroadcast [Int] Shape
    | FReshape Shape
    -- The [Int] is a permutation of [0 .. rank - 1].
    | FTranspose [Int]
    | FConstant Type Tensor
    deriving (Eq, Show)

showUnaryPointwiseOp :: UnaryPointwiseOp -> String
showUnaryPointwiseOp = \case
    Copy -> "copy"
    Sin -> "sin"
    Cos -> "cos"
    Exp -> "exp"
    Negate -> "negate"

showBinaryPointwiseOp :: BinaryPointwiseOp -> String
showBinaryPointwiseOp = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

parseUnaryPointwiseOp :: Parser UnaryPointwiseOp
parseUnaryPointwiseOp =
    Copy <$ try (string "copy")
        <|> Sin <$ try (string "sin")
        <|> Cos <$ try (string "cos")
        <|> Exp <$ try (string "exp")
        <|> Negate <$ try (string "negate")

parseBinaryPointwiseOp :: Parser BinaryPointwiseOp
parseBinaryPointwiseOp =
    Add <$ try (string "+")
        <|> Sub <$ try (string "-")
        <|> Mul <$ try (string "*")
        <|> Div <$ try (string "/")

showFunc :: Func -> [Atom] -> String
showFunc (FUnaryPointwise op) [x] =
    showUnaryPointwiseOp op
        ++ " "
        ++ showVarName (atomName x)
showFunc (FBinaryPointwise op) [x, y] =
    showVarName (atomName x)
        ++ " "
        ++ showBinaryPointwiseOp op
        ++ " "
        ++ showVarName (atomName y)
showFunc FDot [x, y] =
    "dot "
        ++ showVarName (atomName x)
        ++ " "
        ++ showVarName (atomName y)
showFunc (FBroadcast ixs sh) [x] =
    "broadcast "
        ++ show ixs
        ++ " "
        ++ showShape sh
        ++ " "
        ++ showVarName (atomName x)
showFunc (FReshape sh) [x] =
    "reshape "
        ++ showShape sh
        ++ " "
        ++ showVarName (atomName x)
showFunc (FTranspose perm) [x] =
    "transpose "
        ++ show perm
        ++ " "
        ++ showVarName (atomName x)
showFunc (FConstant ty t) [] =
    "constant "
        ++ showType ty
        ++ " "
        ++ show t
showFunc f xs =
    error $
        "Internal error: Cannot show function "
            ++ show f
            ++ " with arguments "
            ++ show xs

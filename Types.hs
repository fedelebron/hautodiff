{-# LANGUAGE FlexibleContexts #-}

module Types (
    BaseType (..),
    showBaseType,
    Shape,
    showShape,
    Type (..),
    showType,
    parseBaseType,
    parseShape,
    parseType,
    applyPermutation,
    applyReshape,
    typeShape
) where

import Control.Applicative ((<|>))
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec.String (Parser)
import Error
import Data.List (sort)
import GHC.Stack
import Control.Monad.Except (MonadError, throwError)
import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint, text, prettyShow)

data BaseType = TBool | TInt | TFloat64 | TFloat32 | TBFloat16 deriving (Eq, Ord, Show)
type Shape = [Int]
data Type = Type Shape BaseType | UnknownType deriving (Eq, Ord, Show)

instance Pretty Type where
    pPrint = text . showType

instance Pretty BaseType where
    pPrint = text . showBaseType

typeShape :: Type -> Shape
typeShape (Type s _) = s
typeShape t = error $ "Cannot compute shape of type " ++ show t

parseShape :: Parser Shape
parseShape =
    let number = many1 digit <* spaces
        listOfNumbers = number `sepBy` (string "," *> spaces)
     in map read <$> (char '[' *> spaces *> listOfNumbers <* char ']')

showShape :: Shape -> String
showShape = show

applyReshape :: (HasCallStack, MonadError Error m) => Shape -> Shape -> m Shape
applyReshape from to =
    if product from == product to
    then return to
    else throwError $ Error ("Cannot reshape from " ++ showShape from ++ " into "
                             ++ showShape to)
                      callStack

applyPermutation :: (HasCallStack, MonadError Error m) => [Int] -> Shape -> m Shape
applyPermutation perm sh =
    let n = length sh
    in  if sort perm == [0 .. n - 1]
        then return [sh !! i | i <- perm]
        else throwError (Error ("Invalid permutation: "
                                ++ show perm ++ " for shape " ++ show perm)
                               callStack)
                        

parseBaseType :: Parser BaseType
parseBaseType =
    (TFloat32 <$ string "f32")
    <|> (TBFloat16 <$ string "bf16")
    <|> (TFloat64 <$ string "f64")
    <|> (TInt <$ string "i")
    <|> (TBool <$ string "b")

showBaseType :: BaseType -> String
showBaseType TBool = "b"
showBaseType TInt = "i"
showBaseType TFloat64 = "f64"
showBaseType TFloat32 = "f32"
showBaseType TBFloat16 = "bf16"

showType :: Type -> String
showType (Type s bt) = prettyShow bt ++ showShape s
showType UnknownType = "Unknown"

parseType :: Parser Type
parseType = flip Type <$> parseBaseType <*> parseShape

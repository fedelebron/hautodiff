module Binding (Binding (..), showBinding, parseBinding) where

import Atom
import Expr

import Text.Parsec.Char (spaces, string)
import Text.Parsec.String (Parser)
import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint, text, prettyShow)

-- A binding of a variable to an expression.
data Binding = Binding
    { atom :: Atom
    , expr :: Expr
    }
    deriving (Eq, Show)

instance Pretty Binding where
    pPrint = text . showBinding

showBinding :: Binding -> String
showBinding (Binding a e) = prettyShow a ++ " = " ++ prettyShow e

parseBinding :: Parser Binding
parseBinding = Binding <$> parseAtom <*> (spaces *> string "=" *> spaces *> parseExpr <* spaces)

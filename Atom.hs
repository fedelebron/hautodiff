module Atom (Atom (..), showAtom, parseAtom) where

import Name
import Types

import Control.Applicative ((<|>))
import Text.Parsec (try)
import Text.Parsec.Char (spaces, string)
import Text.Parsec.String (Parser)
import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint, text, prettyShow)

-- An atom is a typed variable.
data Atom = Atom { atomTy :: Type,
                   atomName :: VarName
                 } deriving (Eq, Ord, Show)

instance Pretty Atom where
  pPrint = text . showAtom

showAtom :: Atom -> String
showAtom (Atom t n) = prettyShow n ++ typeQualification
  where
    typeQualification = case t of
        UnknownType -> ""
        t' -> ": " ++ showType t'

-- We allow optional type qualification, but this is always filled in during
-- type inference. There are no polymorphic variables.
parseAtom :: Parser Atom
parseAtom = flip Atom <$> parseVarName <*> possiblyParseTypeQualification <* spaces
  where
    possiblyParseTypeQualification :: Parser Type
    possiblyParseTypeQualification = try (spaces *> string ":" *> spaces *> parseType) <|> pure UnknownType

{-# LANGUAGE ViewPatterns #-}

module Definition (Definition (..), showDefinition, parseDefinition, makeSampleInput) where

import Atom
import Binding
import Data.List (intercalate)
import Error
import Name
import Tensor
import Types

import Text.Parsec (between)
import Text.Parsec.Char (char, spaces, string)
import Text.Parsec.Combinator (many1, sepBy, sepBy1)
import Text.Parsec.String (Parser)
import Text.PrettyPrint.HughesPJClass (Pretty (..), pPrint, text)

import GHC.Stack

-- A procedure, to be executed top-to-bottom. The returned atoms must refer to
-- bound variables in the body, and the parameters are bound in the body during
-- execution.
data Definition = Definition
    { defName :: FunctionName
    , parameters :: [Atom]
    , body :: [Binding]
    , returned :: [Atom]
    }
    deriving (Eq, Show)

instance Pretty Definition where
  pPrint = text . showDefinition

showDefinition :: Definition -> String
showDefinition (Definition name params bs rets) =
    "def " ++ showFunctionName name ++ "(" ++ paramsStr ++ "):\nlet\t" ++ bindingsStr ++ returnStr
  where
    paramsStr = intercalate ", " (map showAtom params)
    bindingsStr = intercalate "\n\t" (map showBinding bs)
    returnStr = "\nin\t" ++ intercalate ", " (map (showVarName . atomName) rets)

parseDefinition :: Parser Definition
parseDefinition =
    let namePart = string "def" *> spaces *> parseFunctionName
        paramList = parseAtom `sepBy` (string "," *> spaces)
        paramsPart = (spaces *> char '(') *> paramList <* (char ')' *> spaces *> char ':')
        letPart = spaces *> string "let"
        inPart = spaces *> string "in"
        bindingsPart = between letPart inPart (many1 (spaces *> parseBinding))
        returnPart = spaces *> (parseAtom `sepBy1` (char ',' <* spaces))
     in Definition <$> namePart <*> paramsPart <*> bindingsPart <*> returnPart

-- Given a function, returns a pair of (possible inputs, possible cotangents).
makeSampleInput :: HasCallStack => Definition -> Either Error ([Tensor], [Tensor])
makeSampleInput def =
    (,)
        <$> mapM create (parameters def)
        <*> mapM create (returned def)
  where
    create :: Atom -> Either Error Tensor
    create (Atom (Type sh t) _) =
        let n = product sh
         in mkFloatTensor t sh [1.0 .. fromIntegral n]
    create (atomTy -> t) =
        Left $
            Error
                ("Unimplemented sample input for type: " ++ showType t)
                callStack

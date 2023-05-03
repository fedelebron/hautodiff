module TypeInference (inferTypes, checkArgumentTypes, isConstant) where

import Atom
import Binding
import Control.Monad
import Definition
import qualified Environment as E
import Error
import Expr
import Func
import GHC.Stack
import Name
import Tensor
import Types

concreteType :: Atom -> Either Error Type
concreteType (Atom UnknownType name) = Left (Error ("Unknown type for parameter " ++ showVarName name) callStack)
concreteType (Atom t _) = Right t

applyBindingTypes :: E.Env Type -> Definition -> Either Error Definition
applyBindingTypes env def = do
    body' <- mapM applyType (body def)
    returned' <- mapM fixAtom (returned def)
    return $ def { body = body', returned = returned' }
  where
    applyType b = do
        let name = atomName (atom b)
            -- We've already checked that the inferred types exist and
            -- are consistent, so this lookup cannot fail.
            Expr _ f args = expr b
        ty <- E.lookup name env
        argTypes <- mapM ((`E.lookup` env) . atomName) args
        let typedArgs = zipWith (\(Atom _ n) t -> Atom t n) args argTypes
        return $ b { atom = Atom ty name
                     , expr = Expr ty f typedArgs
                   }
    fixAtom (Atom _ name) = flip Atom name <$> E.lookup name env

inferExpressionType :: HasCallStack => Func -> [Type] -> Either Error Type
inferExpressionType f tys = case (f, tys) of
    (FUnaryPointwise _, [t]) -> Right t
    (FBinaryPointwise _, [s, t]) -> do
        assertTrue (s == t) $
            Error
                ( "Inconsistently typed arguments for binary pointwise function "
                    ++ show f
                    ++ ". Arguments have type "
                    ++ showType s
                    ++ " and "
                    ++ showType t
                )
                callStack
        return s
    (FDot, [s, t]) -> case (s, t) of
        (Type sShape sType, Type tShape tType) -> do
            assertTrue (sType == tType) $
                Error
                    ( "Cannot dot tensors of different base type, got "
                        ++ showBaseType sType
                        ++ " and "
                        ++ showBaseType tType
                    )
                    callStack
            case (sShape, tShape) of
                ([n, m], [m', p]) | m == m' -> return (Type [n, p] sType)
                _ ->
                    Left $
                        Error
                            ( "Got incompatible shapes for dot: "
                                ++ showShape sShape
                                ++ " and "
                                ++ showShape tShape
                            )
                            callStack
        _ ->
            error $
                "Internal error during inference of dot, got types "
                    ++ show s
                    ++ " and "
                    ++ show t
    (FBroadcast perm shout, [Type shin bt])
        | all id [shout !! (perm !! i) == shin !! i | i <- perm] ->
            Right (Type shout bt)
    (FReshape sh', [Type sh bt]) -> (`Type` bt) <$> applyReshape sh sh'
    (FTranspose perm, [Type sh bt]) -> (`Type` bt) <$> applyPermutation perm sh
    (FConstant ty _, []) -> Right ty
    _ -> Left (Error ("Unsupported expression for type inference: " ++ show f) callStack)

checkExplicitAtomType :: HasCallStack => E.Env Type -> Atom -> Either Error ()
checkExplicitAtomType env a = do
    inferredType <- E.lookup (atomName a) env
    assertTrue
        ((atomTy a) `elem` [UnknownType, inferredType])
        ( Error
            ( "Inconsistent type for "
                ++ showAtom a
                ++ ". Explicit type is "
                ++ showType (atomTy a)
                ++ ", but inferred type is "
                ++ showType inferredType
            )
            callStack
        )

typeBinding :: HasCallStack => E.Env Type -> Binding -> Either Error (E.Env Type)
typeBinding env b = do
    -- Our invariant is that all variables used as arguments for this expression
    -- already have concrete types, and those types are in `env`.
    let Expr ty f as = expr b
    -- We first check that if explicit argument types were given, they match the
    -- inferred types in our environment.
    mapM_ (checkExplicitAtomType env) as
    -- We now infer the type of the expression, given the function and its
    -- concretely-typed operands.
    concreteOperandTypes <- mapM (flip E.lookup env . atomName) as
    ft <- inferExpressionType f concreteOperandTypes

    assertTrue
        (ty == UnknownType || ft == ty)
        ( Error
            ( "Explicit type for expression in binding "
                ++ showBinding b
                ++ ", "
                ++ showType ty
                ++ ", does not match inferred type, "
                ++ showType ft
            )
            callStack
        )
    let env' = E.insert (atomName (atom b)) ft env

    return env'

inferTypes :: HasCallStack => Definition -> Either Error Definition
inferTypes def = do
    -- Parameter types must be given explicitly.
    paramTypes <- mapM concreteType (parameters def)
    let paramNames = map atomName (parameters def)
    let env = foldr (uncurry E.insert) E.empty (zip paramNames paramTypes)
    bindingTypes <- foldM typeBinding env (body def)
    applyBindingTypes bindingTypes def

checkArgumentTypes :: HasCallStack => [Tensor] -> [Atom] -> Either Error ()
checkArgumentTypes = zipWithM_ checkArgumentType
  where
    checkArgumentType t (Atom (Type sh abt) name) = do
        let tbt = tensorBaseType t
        assertTrue
            ( tbt == abt
                -- Tensors of type TBFloat16 are stored internally as a list
                -- of floats, for which tensorBaseType returns TFloat32. We
                -- thus allow this incompatibility.
                || (tbt == TFloat32 && abt == TBFloat16)
            )
            $ Error
                ( "Tensor for "
                    ++ showVarName name
                    ++ " has base type "
                    ++ showBaseType tbt
                    ++ ", but the parameter has base "
                    ++ "type "
                    ++ showBaseType abt
                )
                callStack
        assertTrue (shape t == sh) $
            Error
                ( "Tensor for "
                    ++ showVarName name
                    ++ " has shape "
                    ++ showShape (shape t)
                    ++ ", but the parameter has "
                    ++ " shape "
                    ++ showShape sh
                )
                callStack
    checkArgumentType t a =
        Left $
            Error
                ( "Internal error: Checking argument type for"
                    ++ " "
                    ++ show t
                    ++ " with parameter "
                    ++ showAtom a
                )
                callStack

isConstant :: E.Env Binding -> VarName -> Bool
isConstant env = go
  where
    go x = case E.lookup x env of
        -- We found an unbound variable, this tree was not a constant!
        Left _ -> False
        -- This is a constant if and only if all its arguments are constants.
        Right b -> all (go . atomName) (params . expr $ b)

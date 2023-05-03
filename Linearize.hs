module Linearize (linearize) where

import Atom
import Binding
import Control.Monad
import Data.List (find)
import Definition
import qualified Environment as E
import Error
import Evaluate
import Expr
import Func
import Name
import Optimizations
import Tensor
import TypeInference
import Types

import GHC.Stack

-- Given p and f, returns (f(p), f'(p)).
linearize :: HasCallStack => [Tensor] -> Definition -> Either Error ([Tensor], Definition)
linearize args def = do
    let params = parameters def
    assertTrue
        (length args == length params)
        ( Error
            ( "Tried to linearize a function of "
                ++ show (length params)
                ++ " parameters, but "
                ++ show (length args)
                ++ " arguments were given."
            )
            callStack
        )
    checkArgumentTypes args params
    let paramNames = map atomName params
        e = foldr (uncurry E.insert) E.empty (zip paramNames args)
        linearDef =
            Definition
                { defName = FunctionName ('d' : unFunctionName (defName def))
                , parameters = parameters def
                , body = []
                , returned = error "Return atoms not set for differential."
                }
        renames = foldr (uncurry E.insert) E.empty (zip paramNames paramNames)
    (e', renames', linearDef') <-
        foldM
            linearizeBinding
            (e, renames, linearDef)
            (body def)
    returnedNameDefs' <- mapM (flip E.lookup renames' . atomName) (returned def)
    let getNewBinding name = find ((== name) . atomName . atom) (body linearDef')
        returnBindingDefs' = mapM getNewBinding returnedNameDefs' :: Maybe [Binding]
    returnAtomDefs' <-
        maybe
            ( Left $
                Error
                    "Linearization did not preserve the return value!"
                    callStack
            )
            (return . map atom)
            returnBindingDefs'
    let linearDef'' = linearDef'{returned = returnAtomDefs'}
    rets <- mapM (flip E.lookup e' . atomName) (returned def)
    return (rets, linearDef'')

linearizeBinding ::
    HasCallStack =>
    (E.Env Tensor, E.Env VarName, Definition) ->
    Binding ->
    Either Error (E.Env Tensor, E.Env VarName, Definition)
linearizeBinding (env, renames, df) (Binding atom expr) = do
    (res, df', newName) <- linearizeExpr df env renames expr
    let env' = E.insert (atomName atom) res env
    let renames' = E.insert (atomName atom) newName renames
    return (env', renames', df')

linearizeExpr ::
    HasCallStack =>
    Definition ->
    E.Env Tensor ->
    E.Env VarName ->
    Expr ->
    Either Error (Tensor, Definition, VarName)
linearizeExpr df env renames (Expr ty f arguments) = do
    primals <- mapM ((`E.lookup` env) . atomName) arguments
    -- renames point to the roots of the tangent expressions, for each primal
    -- expression.
    tangentNames <- mapM ((`E.lookup` renames) . atomName) arguments
    let tangents = zipWith (\a' (Atom t _) -> Atom t a') tangentNames arguments
    let res = runFunc f primals
    (newAtom, bs) <- linearizeFunc df primals tangents ty f
    let df' = df{body = body df ++ bs}
    return (res, df', atomName newAtom)

isLinearFunc :: Func -> Bool
isLinearFunc (FBinaryPointwise o) | o `elem` [Add, Sub] = True
isLinearFunc (FUnaryPointwise o) | o `elem` [Copy, Negate] = True
isLinearFunc (FBroadcast _ _) = True
isLinearFunc (FReshape _) = True
isLinearFunc (FTranspose _) = True
isLinearFunc _ = False

linearizeFunc :: HasCallStack => Definition -> [Tensor] -> [Atom] -> Type -> Func -> Either Error (Atom, [Binding])
linearizeFunc df primals tangents ty f = case (f, primals, tangents) of
    (_, _, dxs) | isLinearFunc f -> return . runBindingMonad df $ do
        a1 <- addBinding ty f dxs
        return a1
    (FUnaryPointwise Sin, [x], [dx]) -> return . runBindingMonad df $ do
        a1 <- addBinding ty (FConstant ty x) []
        a2 <- addBinding ty (FUnaryPointwise Cos) [a1]
        a3 <- addBinding ty (FBinaryPointwise Mul) [a2, dx]
        return a3
    (FUnaryPointwise Cos, [x], [dx]) -> return . runBindingMonad df $ do
        a1 <- addBinding ty (FConstant ty x) []
        a2 <- addBinding ty (FUnaryPointwise Sin) [a1]
        a3 <- addBinding ty (FUnaryPointwise Negate) [a2]
        a4 <- addBinding ty (FBinaryPointwise Mul) [a3, dx]
        return a4
    (FUnaryPointwise Exp, [x], [dx]) -> return . runBindingMonad df $ do
        a1 <- addBinding ty (FConstant ty x) []
        a2 <- addBinding ty (FUnaryPointwise Exp) [a1]
        a3 <- addBinding ty (FBinaryPointwise Mul) [a2, dx]
        return a3
    (FBinaryPointwise Mul, [x, y], [dx, dy]) -> return . runBindingMonad df $ do
        -- d(x * y)_p = dx_p * y + x * dy_p
        -- Though by convention, we only put tangents on the right argument,
        -- while the left argument is a primal.
        a1 <- addBinding ty (FConstant ty y) []
        a2 <- addBinding ty (FBinaryPointwise Mul) [a1, dx]
        a3 <- addBinding ty (FConstant ty x) []
        a4 <- addBinding ty (FBinaryPointwise Mul) [a3, dy]
        a5 <- addBinding ty (FBinaryPointwise Add) [a2, a4]
        return a5
    (FBinaryPointwise Div, [_, y], [dx, dy]) -> do
        -- To linearize a / b, b must be a constant.
        assertTrue (isConstant (E.fromDefinition df) (atomName dy)) $
            Error
                ("Cannot linearize a division with a non-constant denominator.")
                callStack
        return . runBindingMonad df $ do
            a1 <- addBinding ty (FConstant ty (invTensor y)) []
            a2 <- addBinding ty (FBinaryPointwise Mul) [a1, dx]
            return a2
    (FConstant _ _, _, _) -> return . runBindingMonad df $ do
        a1 <- addBinding ty (FConstant ty (makeZeros ty)) []
        return a1
    (FDot, [x, y], [dx, dy])
        | Type [n, m] bt <- atomTy dx
        , Type [_, p] _ <- atomTy dy -> return . runBindingMonad df $ do
            let x0t = Type [n, m] bt
                y0t = Type [m, p] bt
            x0 <- addBinding x0t (FConstant x0t x) []
            y0 <- addBinding y0t (FConstant y0t y) []
            x0dy <- addBinding ty FDot [x0, dy]
            dxy0 <- addBinding ty FDot [dx, y0]
            r <- addBinding ty (FBinaryPointwise Add) [x0dy, dxy0]
            return r
    _ -> Left $ Error ("Unimplemented linearization for " ++ show f) callStack

{-# LANGUAGE ExplicitForAll #-}

module Evaluate (evaluate, runBinding, runFunc) where

import Binding
import Control.Monad (foldM)
import Definition
import qualified Environment as E
import Error
import Expr
import Func
import GHC.Stack
import Tensor
import TypeInference
import Types

-- Given a function definition and some tensors for its parameters, returns the
-- result of evaluating the function. There is one returned tensor per returned
-- variable in the definition.
evaluate :: HasCallStack => Definition -> [Tensor] -> Either Error [Tensor]
evaluate def args = do
    let ps = parameters def
    assertTrue
        (length args == length ps)
        ( Error
            ( "Tried to evaluate a function of "
                ++ show (length ps)
                ++ " parameters, but "
                ++ show (length args)
                ++ " arguments were given."
            )
            callStack
        )
    checkArgumentTypes args ps
    let paramNames = map atomName ps
        -- We add all parameters to the environment, mapped to the tensor
        -- values given.
        e = foldr (uncurry E.insert) E.empty (zip paramNames args)
    -- We run every binding in order.
    e' <- foldM runBinding e (body def)
    -- Recover from the binding environment each of the returned tensors.
    mapM (flip E.lookup e' . atomName) (returned def)

-- Runs a binding in an existing environment, returning the environment after
-- having run the binding.
runBinding :: HasCallStack => E.Env Tensor -> Binding -> Either Error (E.Env Tensor)
runBinding env b@(Binding atom expr) = do
    v <- runExpr expr env
    assertTrue (shape v == typeShape (exprType expr)) $
        Error
            ( "Incorrect shape for evaluation result! It should be "
                ++ show (typeShape (exprType expr))
                ++ ", but it is "
                ++ show (shape v)
                ++ ". During evaluation of binding "
                ++ showBinding b
            )
            callStack
    return $ flip (E.insert (atomName atom)) env v

-- Evaluates an expression, given a binding environment.
runExpr :: Expr -> E.Env Tensor -> Either Error Tensor
runExpr (Expr _ f args) env = do
    runFunc f <$> mapM ((`E.lookup` env) . atomName) args

runFunc :: Func -> [Tensor] -> Tensor
runFunc f args = case (f, args) of
    (FUnaryPointwise op, [x]) -> evaluateUnaryPointwise op x
    (FBinaryPointwise op, [x, y]) -> evaluateBinaryPointwise op x y
    (FDot, [x, y]) -> dot x y
    (FBroadcast ixs sh, [x]) -> broadcast ixs sh x
    (FReshape sh, [x]) -> reshape sh x
    (FTranspose ixs, [x]) -> transpose ixs x
    (FConstant _ t, []) -> t
    _ -> error $ "Unsupported expression being evaluated: " ++ show f

-- These should be generalized.
evaluateUnaryPointwise :: UnaryPointwiseOp -> Tensor -> Tensor
evaluateUnaryPointwise op = op'
  where
    op' = case op of
        Copy -> id
        Cos -> cos
        Sin -> sin
        Exp -> exp
        Negate -> negate

evaluateBinaryPointwise :: BinaryPointwiseOp -> Tensor -> Tensor -> Tensor
evaluateBinaryPointwise op = op'
  where
    op' = case op of
        Add -> (+)
        Sub -> (-)
        Mul -> (*)
        Div -> (/)

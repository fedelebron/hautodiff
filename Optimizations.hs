module Optimizations (
    forwardCopies,
    eliminateDeadCode,
    constantFoldDef,
    runBindingMonad,
    freshAtom,
    addBinding,
) where

import Binding
import Control.Monad.State
import Definition
import qualified Environment as E
import Evaluate
import Expr
import Func
import Name
import Tensor
import Types
import Utils

data BindingState = BindingState VarName [Binding]

runBindingMonad :: Definition -> State BindingState a -> (a, [Binding])
runBindingMonad df s =
    let (a, BindingState _ bs) = runState s (BindingState m [])
     in (a, bs)
  where
    m = 1 + maximum (map atomName (map atom (body df) ++ parameters df))

freshAtom :: Type -> State BindingState Atom
freshAtom ty = do
    BindingState next bs <- get
    put (BindingState (next + 1) bs)
    return (Atom ty (next + 1))

addBinding :: Type -> Func -> [Atom] -> State BindingState Atom
addBinding ty f as = do
    a <- freshAtom ty
    let b = Binding a (Expr (atomTy a) f as)
    modify (\(BindingState n bs) -> BindingState n (bs ++ [b]))
    return a

constantFoldDef :: Definition -> Definition
constantFoldDef def =
    let binds = body def
        values = foldl constantFold E.empty binds
        binds' = map (makeConstant values) binds
     in def{body = binds'}
  where
    constantFold :: E.Env Tensor -> Binding -> E.Env Tensor
    constantFold env b = either (const env) id (runBinding env b)

    makeConstant :: E.Env Tensor -> Binding -> Binding
    makeConstant env b@(Binding a@(Atom _ name) (Expr t _ _))
        | Right tensor <- E.lookup name env = Binding a (Expr t (FConstant t tensor) [])
        | otherwise = b

forwardCopies :: Definition -> Definition
forwardCopies def =
    let bs = body def
        rets = returned def
        (rets', bs') = go rets bs []
        def' = def{body = bs', returned = rets'}
     in def'
  where
    go rets (Binding a (Expr _ (FUnaryPointwise Copy) [z]) : bs) bs' =
        let bs'' = map (forwardBinding a z) bs'
            rets' = replace a z rets
         in go rets' (map (forwardBinding a z) bs) bs''
    go rets (b : bs) bs' = go rets bs (b : bs')
    go rets [] bs' = (rets, reverse bs')
    forwardBinding a z (Binding a' (Expr t f args)) =
        Binding a' (Expr t f (replace a z args))

eliminateDeadCode :: Definition -> Definition
eliminateDeadCode def =
    let initialUsed = map atomName (returned def)
        binds = body def
        -- This isn't really efficient, since we could do
        -- this in a single pass instead of two, but meh.
        used' = foldr collectUsedNames initialUsed binds
        binds' = filter ((`elem` used') . atomName . atom) binds
     in def{body = binds'}
  where
    collectUsedNames b usedNames
        | atomName (atom b) `elem` usedNames =
            let ps = params (expr b)
             in usedNames ++ map atomName ps
        | otherwise = usedNames

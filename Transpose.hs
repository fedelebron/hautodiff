module Transpose (transposeDef) where

import Atom
import Binding
import Control.Monad.State
import Control.Monad.Except (throwError)
import Definition
import qualified Environment as E
import Error
import Expr
import Func
import GHC.Stack
import Name
import Optimizations
import Types
import Utils (inversePerm)

data CotangentState = CotangentState
    { -- The next free variable name.
      nextVarName :: VarName
    , -- A list of bindings for the transposed program.
      bindings :: [Binding]
    , -- A mapping from variable name in the linear program, to cotangents in the
      -- transposed program. The semantics are that if (x, xs) is in the map, then
      -- the cotangent of x is the sum of all xs.
      cotangentMap :: E.Env [Atom]
    }

freshCotangent :: State CotangentState VarName
freshCotangent = do
    n <- gets nextVarName
    modify (\s -> s{nextVarName = n + 1})
    return n

addCotangent :: VarName -> Atom -> State CotangentState ()
addCotangent v a = do
    m <- gets cotangentMap
    let a' = case E.lookup v m of
            Left _ -> [a]
            Right as -> a : as
    modify (\s -> s{cotangentMap = E.insert v a' m})
    return ()

addCotangentBinding :: Type -> Func -> [Atom] -> State CotangentState Atom
addCotangentBinding ty f as = do
    a <- Atom ty <$> freshCotangent
    _ <- addCotangentBinding' a ty f as
    return a

addCotangentBinding' :: Atom -> Type -> Func -> [Atom] -> State CotangentState Atom
addCotangentBinding' a ty f as = do
    let b = Binding a (Expr ty f as)
    modify (\s -> s{bindings = bindings s ++ [b]})
    return a

runCotangent :: Monad m => Definition -> StateT CotangentState m a -> m CotangentState
runCotangent (Definition _ ps bs _) s =
    let cotangentParam a = (atomName a, [a])
        cotangents = foldr (uncurry E.insert . cotangentParam) E.empty ps
        nextVar = 1 + maximum (map atomName (map atom bs ++ ps))
        initialState = CotangentState nextVar [] cotangents
     in execStateT s initialState

transposeDef :: HasCallStack => Definition -> Either Error Definition
transposeDef (Definition dname ps bs returns) = do
    -- initialCotangents is a map from the old definition's atoms, to the atoms
    -- representing their cotangents in the new program. In
    -- "You only linearize once" parlance, this is a map from v to \ddot{v}.
    -- Initially, we only know the cotangents for the input program's return
    -- values, and those are the transposed function's arguments.
    let initialDefinition =
            Definition
                { defName = FunctionName (unFunctionName dname ++ "t")
                , parameters = returns
                , body = []
                , returned = ps
                }
    let transposeAllBindings = mapM_ transposeBinding (reverse bs)
        transposeArgs = forwardCotangentsToParameters ps
    s <- runCotangent initialDefinition (transposeAllBindings >> transposeArgs)
    let df' = initialDefinition{body = bindings s}
        df'' = eliminateDeadCode $ forwardCopies df'
    return df''

maybeGetCotangentVars :: HasCallStack => VarName -> State CotangentState (Maybe [Atom])
maybeGetCotangentVars v = do
    cotangents <- gets cotangentMap
    return $ case E.lookup v cotangents of
        Left _ -> Nothing
        Right ct -> Just ct

appendCotangents :: HasCallStack => Atom -> Func -> [Atom] -> StateT CotangentState (Either Error) ()
appendCotangents _ _ [] = return ()
appendCotangents dLdZ (FBinaryPointwise Add) [x, y] = cannotFail $ do
  -- If Z = X + Y, then dL/dX = dL/dZ, and dL/dY = dL/dZ.
  addCotangent (atomName x) dLdZ
  addCotangent (atomName y) dLdZ
appendCotangents dLdZ (FBinaryPointwise Sub) [x, y] = cannotFail $ do
  -- If Z = X - Y, then dL/dX = dL/dZ, and dL/dY = -dL/dZ.
  addCotangent (atomName x) dLdZ
  ct <- addCotangentBinding (atomTy dLdZ) (FUnaryPointwise Negate) [dLdZ]
  addCotangent (atomName y) ct
appendCotangents dLdZ (FBinaryPointwise Mul) [x, y] = cannotFail $ do
  -- By convention, x is a constant, and y is the actual tangent in the linear
  -- program. Thus if Z = X . Y, we have dL/dX = 0, and dL/dY = X . dL/dZ.
  ct <- addCotangentBinding (atomTy dLdZ) (FBinaryPointwise Mul) [x, dLdZ]
  addCotangent (atomName y) ct
-- Note Div can't appear in a linear program, as we linearized divs to muls of
-- the multiplicative inverse
appendCotangents dLdZ (FUnaryPointwise Copy) [x] = cannotFail $ do
  -- This is the identity function. Hence,
  addCotangent (atomName x) dLdZ
appendCotangents dLdZ (FUnaryPointwise Negate) [x] = cannotFail $ do
  -- If Z = -X, then dL/dX = -dL/dZ.
  ct <- addCotangentBinding (atomTy dLdZ) (FUnaryPointwise Negate) [dLdZ]
  addCotangent (atomName x) ct
appendCotangents dLdZ (FTranspose perm) [x] = cannotFail $ do
  -- If Z = transpose p X, then dL/dX = transpose p^{-1} dL/dZ.
  ct <- addCotangentBinding (atomTy x) (FTranspose (inversePerm perm)) [dLdZ]
  addCotangent (atomName x) ct
appendCotangents dLdZ FDot [x, y] | Type [n, m] bt <- atomTy x,
                                    Type [_, p] _ <- atomTy y = cannotFail $ do
  -- Note that both x and y can be variables. We'll pull back the cotangent
  -- through both of them, which raises the question of what happens when one
  -- is a constant, since dL/dc = 0 for constants c. Fear not, dear reader,
  -- since we discharge cotangent sums when a variable is created (remember,
  -- we're walking the original program _backwards_!). Thus, when we see a
  -- constant binding in the original program, we'll simply not discharge the
  -- cotangent sums for it.
  -- A calculation best done over a glass of scotch yields:
  -- If Z = XY, then dL/dX = dL/dZ Y^T, dL/dY = X^T dL/dZ.
  yt <- addCotangentBinding (Type [p, m] bt) (FTranspose [1, 0]) [y]
  dLdzyt <- addCotangentBinding (atomTy x) FDot [dLdZ, yt]
  addCotangent (atomName x) dLdzyt
  xt <- addCotangentBinding (Type [m, n] bt) (FTranspose [1, 0]) [x]
  xtdLdz <- addCotangentBinding (atomTy y) FDot [xt, dLdZ] 
  addCotangent (atomName y) xtdLdz
appendCotangents dLdZ (FReshape _) [x]
  | t@(Type sh _) <- atomTy x = cannotFail $ do
  ct <- addCotangentBinding t (FReshape sh) [dLdZ]
  addCotangent (atomName x) ct
-- TODO: Broadcast. Ugh. Gross.
appendCotangents _ f as = throwError $ Error ("Unimplemented vjp of " ++ showFunc f as) callStack

-- We are given dL/dX, and a list of n summands xs, with the interpretation that
-- dL/dX = sum xs. We create n - 1 sums to express this, or an identity function
-- (copy) if n = 1.
-- We write this binding of dL/dX to the environment.
cascadeCotangentBindings :: HasCallStack => Atom -> [Atom] -> StateT CotangentState (Either Error) ()
cascadeCotangentBindings dLdX [] =
   throwError $ Error ("Cannot happen! No cotangents found for " ++ showAtom dLdX) callStack
cascadeCotangentBindings a [summand] = cannotFail $ do
    _ <- addCotangentBinding' a (atomTy a) (FUnaryPointwise Copy) [summand]
    return ()
cascadeCotangentBindings a (summand1 : summand2 : summands) = cannotFail $ do
    -- Not super efficient, but meh. Chain of additions, not a binary tree.
    s <- foldM oneSum summand2 summands
    _ <- addCotangentBinding' a (atomTy a) (FBinaryPointwise Add) [summand1, s]
    return ()
  where
    oneSum s1 s2 = addCotangentBinding (atomTy s1) (FBinaryPointwise Add) [s1, s2]

-- Given some parameters for the procedure, writes cotangent bindings for each
-- of them.
-- N.B. We need this because for body variables, we write their cotangent sums
-- when, while walking the original definition backwards, we find the variable's
-- binding. Since procedure parameters have no bindings in the body, we never
-- actually discharge those cotangent sums, so we do that here.
forwardCotangentsToParameters :: [Atom] -> StateT CotangentState (Either Error) ()
forwardCotangentsToParameters = mapM_ forwardToParameter
  where
    forwardToParameter :: Atom -> StateT CotangentState (Either Error) ()
    forwardToParameter a = do
        m <- gets cotangentMap
        cts <- lift $ E.lookup (atomName a) m
        cascadeCotangentBindings a cts

transposeBinding :: HasCallStack => Binding -> StateT CotangentState (Either Error) ()
transposeBinding b@(Binding _ (Expr _ (FConstant _ _) [])) = do
    -- Constants get added identically to the transposed program. Importantly,
    -- they are hoisted to the top of the definition, since we are walking
    -- bindings backwards, but we must have them present by the time they are
    -- used.
    bs <- gets bindings
    modify (\s -> s{bindings = b : bs})
transposeBinding (Binding atom (Expr _ f as)) = do
    -- Given v, get dL/dv = \ddot{v}.
    ctAtoms <- cannotFail $ maybeGetCotangentVars (atomName atom)
    -- If we had already made dL/dv be a copy of someone else, copy it now,
    -- because it's about to be used to pull it back along f.
    case ctAtoms of
        -- As a somewhat-hack, the initial parameters are their own cotangents,
        -- so we don't want to say "x = copy x" for them.
        Just cts
            | cts /= [atom] -> cascadeCotangentBindings atom cts
            | otherwise -> return ()
        _ -> throwError (Error "Drops aren't implemented." callStack)
    -- Now pull back dL/dV along f for each argument.
    appendCotangents atom f as

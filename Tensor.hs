{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Tensor (
    Tensor,
    Shape,
    shape,
    dot,
    broadcast,
    transpose,
    reshape,
    tensorBaseType,
    mkFloatTensor,
    makeZeros,
    invTensor,
) where

import qualified Data.Array.Dynamic as A
import Error
import GHC.Stack
import Text.PrettyPrint.HughesPJClass
import Types

data Tensor
    = Floats (A.Array Float)
    | Doubles (A.Array Double)
    | Ints (A.Array Integer)
    | Bools (A.Array Bool)
    deriving (Eq)

mkFloatTensor :: HasCallStack => BaseType -> Shape -> [Float] -> Either Error Tensor
mkFloatTensor bt sh fs = do
    assertTrue (product sh == length fs) $
        Error
            ( "Incorrect number of items to create a float Tensor, got "
                ++ show (length fs)
                ++ ", but desired shape is "
                ++ showShape sh
            )
            callStack
    assertTrue (bt `elem` [TFloat32, TBFloat16]) $
        Error
            ( "Cannot make a vector of base type "
                ++ showBaseType bt
                ++ " given a list of floats."
            )
            callStack
    return (Floats (A.fromList sh fs))

instance Pretty Tensor where
    pPrint (Bools x) = pPrint x
    pPrint (Ints x) = pPrint x
    pPrint (Floats x) = pPrint x
    pPrint (Doubles x) = pPrint x

instance Show Tensor where
    show (Bools xs) = show (A.toList xs)
    show (Ints xs) = show (A.toList xs)
    show (Floats xs) = show (A.toList xs)
    show (Doubles xs) = show (A.toList xs)

shape :: Tensor -> Shape
shape (Bools x) = A.shapeL x
shape (Ints x) = A.shapeL x
shape (Floats x) = A.shapeL x
shape (Doubles x) = A.shapeL x

tensorBaseType :: Tensor -> BaseType
tensorBaseType (Bools _) = TBool
tensorBaseType (Ints _) = TInt
tensorBaseType (Floats _) = TFloat32
tensorBaseType (Doubles _) = TFloat64

numPointwise :: String -> (forall a. Num a => a -> a) -> Tensor -> Tensor
numPointwise _ f (Floats x) = Floats (A.mapA f x)
numPointwise _ f (Doubles x) = Doubles (A.mapA f x)
numPointwise _ f (Ints x) = Ints (A.mapA f x)
numPointwise name _ x = error $ "Cannot perform " ++ name ++ " on the array " ++ show x

numPointwise2 :: String -> (forall a. Num a => a -> a -> a) -> Tensor -> Tensor -> Tensor
numPointwise2 _ f (Floats x) (Floats y) = Floats (A.zipWithA f x y)
numPointwise2 _ f (Doubles x) (Doubles y) = Doubles (A.zipWithA f x y)
numPointwise2 _ f (Ints x) (Ints y) = Ints (A.zipWithA f x y)
numPointwise2 name _ x y = error $ "Cannot perform " ++ name ++ " on the two arrays: " ++ show x ++ " and " ++ show y

instance Num Tensor where
    (+) = numPointwise2 "sum" (+)
    (-) = numPointwise2 "subtraction" (-)
    (*) = numPointwise2 "multiplication" (*)
    negate = numPointwise "negate" negate
    abs = numPointwise "abs" abs
    signum = numPointwise "signum" signum
    fromInteger x = Ints (A.scalar x)

floatingPointwise :: String -> (forall a. Floating a => a -> a) -> Tensor -> Tensor
floatingPointwise _ f (Floats x) = Floats (A.mapA f x)
floatingPointwise _ f (Doubles x) = Doubles (A.mapA f x)
floatingPointwise name _ x = error $ "Cannot perform " ++ name ++ " on the array " ++ show x

instance Floating Tensor where
    log = floatingPointwise "log" log
    cos = floatingPointwise "cos" cos
    sin = floatingPointwise "sin" sin
    exp = floatingPointwise "exp" exp
    asin = floatingPointwise "asin" asin
    acos = floatingPointwise "acos" acos
    atan = floatingPointwise "atan" atan
    sinh = floatingPointwise "sinh" sinh
    cosh = floatingPointwise "cosh" cosh
    asinh = floatingPointwise "asinh" asinh
    acosh = floatingPointwise "acosh" acosh
    atanh = floatingPointwise "atanh" atanh
    pi = error "why"

fractionalPointwise2 :: String -> (forall a. Fractional a => a -> a -> a) -> Tensor -> Tensor -> Tensor
fractionalPointwise2 _ f (Floats x) (Floats y) = Floats (A.zipWithA f x y)
fractionalPointwise2 _ f (Doubles x) (Doubles y) = Doubles (A.zipWithA f x y)
fractionalPointwise2 name _ x y = error $ "Cannot perform " ++ name ++ " on the arrays " ++ show x ++ " and " ++ show y

instance Fractional Tensor where
    (/) = fractionalPointwise2 "div" (/)
    fromRational = error "Conversion from a rational to a Tensor is not implemented."

broadcast :: [Int] -> Shape -> Tensor -> Tensor
broadcast ixs sh = \case
    Bools xs -> Bools (A.broadcast ixs sh xs)
    Ints xs -> Ints (A.broadcast ixs sh xs)
    Floats xs -> Floats (A.broadcast ixs sh xs)
    Doubles xs -> Doubles (A.broadcast ixs sh xs)

reshape :: Shape -> Tensor -> Tensor
reshape sh = \case
    Bools xs -> Bools (A.reshape sh xs)
    Ints xs -> Ints (A.reshape sh xs)
    Floats xs -> Floats (A.reshape sh xs)
    Doubles xs -> Doubles (A.reshape sh xs)

transpose :: [Int] -> Tensor -> Tensor
transpose ixs = \case
    Bools xs -> Bools (A.transpose ixs xs)
    Ints xs -> Ints (A.transpose ixs xs)
    Floats xs -> Floats (A.transpose ixs xs)
    Doubles xs -> Doubles (A.transpose ixs xs)

dot :: HasCallStack => Tensor -> Tensor -> Tensor
dot (Ints xs) (Ints ys) = Ints (xs `matmul` ys)
dot (Floats xs) (Floats ys) = Floats (xs `matmul` ys)
dot (Doubles xs) (Doubles ys) = Doubles (xs `matmul` ys)
dot s t =
    error
        ( "Incompatible tensors for dot: "
            ++ show s
            ++ " and "
            ++ show t
            ++ ". Stack: "
            ++ prettyCallStack callStack
        )

matmul :: HasCallStack => Num a => A.Array a -> A.Array a -> A.Array a
matmul x y
    | [m, n] <- shX
    , [n', o] <- shY
    , n == n' =
        A.generate [m, o] $ \ixs ->
            case ixs of
                [i, j] ->
                    let xv = A.reshape [n] $ A.slice [(i, 1), (0, n)] x
                        yv = A.reshape [n] $ A.slice [(0, n), (j, 1)] y
                     in sum $ A.zipWithA (*) xv yv
                _ -> error "Impossible"
    | otherwise =
        error $
            "matmul: bad shapes "
                ++ show shX
                ++ " * "
                ++ show shY
                ++ ". Stack: "
                ++ prettyCallStack callStack
  where
    shX = A.shapeL x
    shY = A.shapeL y

makeZeros :: Type -> Tensor
makeZeros (Type sh bty) =
    let n = product sh
        zeros k = A.fromList sh (replicate n k)
     in case bty of
            TBFloat16 -> Floats $ zeros 0.0
            TFloat32 -> Floats $ zeros 0.0
            TFloat64 -> Doubles $ zeros 0.0
            TInt -> Ints $ zeros 0
            TBool -> Bools $ zeros False
makeZeros t = error ("Internal error: Can't make errors for type: " ++ showType t)

invTensor :: Tensor -> Tensor
invTensor (Floats xs) = Floats (A.mapA (1.0 /) xs)
invTensor (Doubles xs) = Doubles (A.mapA (1.0 /) xs)
invTensor t = error ("Cannot invert tensor: " ++ show t)

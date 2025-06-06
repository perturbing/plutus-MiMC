{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.BlsUtils (
    bls12_381_base_prime,
    bls12_381_scalar_prime,
    MultiplicativeGroup (..),
    -- Scalar type and functions
    Scalar (..),
    mkScalar,
    powerOfTwoExponentiationScalar,
    negateScalar,
    -- Fp type and functions
    Fp (..),
    mkFp,
    powerOfTwoExponentiationFp,
    negateFp,
    -- Fp2 type and functions
    Fp2 (..),
    powModFp2,
    powerOfTwoExponentiationFp2,
    negateFp2,
) where

import PlutusTx (makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Builtins (
    BuiltinBLS12_381_G1_Element,
    BuiltinBLS12_381_G2_Element,
    bls12_381_G1_add,
    bls12_381_G1_compress,
    bls12_381_G1_compressed_zero,
    bls12_381_G1_neg,
    bls12_381_G1_scalarMul,
    bls12_381_G1_uncompress,
    bls12_381_G2_add,
    bls12_381_G2_compress,
    bls12_381_G2_compressed_zero,
    bls12_381_G2_neg,
    bls12_381_G2_scalarMul,
    bls12_381_G2_uncompress,
    expModInteger,
 )
import PlutusTx.Numeric (
    AdditiveGroup (..),
    AdditiveMonoid (..),
    AdditiveSemigroup (..),
    Module (..),
    MultiplicativeMonoid (..),
    MultiplicativeSemigroup (..),
 )
import PlutusTx.Prelude (
    Bool (..),
    Eq (..),
    Integer,
    Ord ((<), (<=)),
    divide,
    error,
    even,
    modulo,
    not,
    otherwise,
    ($),
    (&&),
    (.),
    (<>),
    (>),
    (||),
 )
import qualified Prelude as Haskell

-- In this module, we setup the two prime order fields for BLS12-381.
-- as the type Fp/Fp2 (base points) and Scalar.
-- Note that for safety, both the Scalar and Fp constructors
-- are not exposed. Instead, the mkScalar and mkFp suffice,
-- which fail in a script if an integer provided that is negative.

-- The prime order of the generator in the field. So, g^order = id,
bls12_381_scalar_prime :: Integer
bls12_381_scalar_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

-- The prime of the base field. So for a g on the curve, its
-- x and y coordinates are elements of the base field.
bls12_381_base_prime :: Integer
bls12_381_base_prime = 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787

newtype Scalar = Scalar {unScalar :: Integer} deriving (Haskell.Show)
makeLift ''Scalar
makeIsDataIndexed ''Scalar [('Scalar, 0)]

-- Exclude for safety negative integers and integers large/equal
-- to the field prime. This is the primary interface to work with
-- the Scalar type onchain. This is for security reasons,
-- to make sure provided objects are field elements.
{-# INLINEABLE mkScalar #-}
mkScalar :: Integer -> Scalar
mkScalar n = if 0 <= n && n < bls12_381_scalar_prime then Scalar n else error ()

instance Eq Scalar where
    {-# INLINEABLE (==) #-}
    Scalar a == Scalar b = a == b

instance AdditiveSemigroup Scalar where
    {-# INLINEABLE (+) #-}
    (+) (Scalar a) (Scalar b) = Scalar $ (a + b) `modulo` bls12_381_scalar_prime

instance AdditiveMonoid Scalar where
    {-# INLINEABLE zero #-}
    zero = Scalar 0

-- Note that PlutusTx.Numeric implements negate for an additive group. This is
-- canonically defined as zero - x. But not that a more efficient way to do it
-- in plutus is by calculating it as: inv (Scalar x) = Scalar $ bls12_381_scalar_prime - x
-- saving a modulo operation (not considering 0 here).
instance AdditiveGroup Scalar where
    {-# INLINEABLE (-) #-}
    (-) (Scalar a) (Scalar b) = Scalar $ (a - b) `modulo` bls12_381_scalar_prime

-- This is a more efficient way to calculate the additive inverse
-- Be sure that you are using this one instead of the one from PlutusTx.Numeric.
{-# INLINEABLE negateScalar #-}
negateScalar :: Scalar -> Scalar
negateScalar (Scalar x) = if x == 0 then Scalar 0 else Scalar $ bls12_381_scalar_prime - x

instance MultiplicativeSemigroup Scalar where
    {-# INLINEABLE (*) #-}
    (*) (Scalar a) (Scalar b) = Scalar $ (a * b) `modulo` bls12_381_scalar_prime

instance MultiplicativeMonoid Scalar where
    {-# INLINEABLE one #-}
    one = Scalar 1

-- Since plutus 1.9, PlutusTx.Numeric does not implement a Multiplicative group anymore.
-- But since we use a field, multiplicative inversion is well-defined if we exclude 0.
-- We also implement the reciprocal (the multiplicative inverse of an element in the group).
-- For the additive group, there is negate function in PlutusTx.Numeric for the additive inverse.
class (MultiplicativeMonoid a) => MultiplicativeGroup a where
    div :: a -> a -> a
    recip :: a -> a

-- In math this is b^a mod p, where b is of type scalar and a any integer
instance Module Integer Scalar where
    {-# INLINEABLE scale #-}
    scale :: Integer -> Scalar -> Scalar
    scale e (Scalar b) = Scalar $ expModInteger b e bls12_381_scalar_prime

-- Implementing the multiplicative group for the scalar type via extended euclidean method
instance MultiplicativeGroup Scalar where
    {-# INLINEABLE recip #-}
    recip (Scalar a) = Scalar $ expModInteger a (-1) bls12_381_scalar_prime
    {-# INLINEABLE div #-}
    div a b = a * recip b

-- This is a special case of modular exponentiation, where the exponent is a power of two.
-- This saves alot of script budget. Here we mean for x^e, that e = 2^k
{-# INLINEABLE powerOfTwoExponentiationScalar #-}
powerOfTwoExponentiationScalar :: Scalar -> Integer -> Scalar
powerOfTwoExponentiationScalar x k = if k < 0 then error () else go x k
  where
    go x' k' =
        if k' == 0
            then x'
            else powerOfTwoExponentiationScalar (x' * x') (k' - 1)

-- Implementing an additive group for both the G1 and G2 elements.

instance AdditiveSemigroup BuiltinBLS12_381_G1_Element where
    {-# INLINEABLE (+) #-}
    (+) = bls12_381_G1_add

instance AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINEABLE zero #-}
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

instance AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINEABLE (-) #-}
    (-) a b = a + bls12_381_G1_neg b

instance Module Scalar BuiltinBLS12_381_G1_Element where
    {-# INLINEABLE scale #-}
    scale (Scalar a) = bls12_381_G1_scalarMul a

instance AdditiveSemigroup BuiltinBLS12_381_G2_Element where
    {-# INLINEABLE (+) #-}
    (+) = bls12_381_G2_add

instance AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINEABLE zero #-}
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINEABLE (-) #-}
    (-) a b = a + bls12_381_G2_neg b

instance Module Scalar BuiltinBLS12_381_G2_Element where
    {-# INLINEABLE scale #-}
    scale (Scalar a) = bls12_381_G2_scalarMul a

-- Implementing the base field Fp.

-- The field elements are the x and y coordinates of the points on the curve.
newtype Fp = Fp {unFp :: Integer} deriving (Haskell.Show)
makeLift ''Fp
makeIsDataIndexed ''Fp [('Fp, 0)]

{-# INLINEABLE mkFp #-}
mkFp :: Integer -> Fp
mkFp n = if 0 <= n && n < bls12_381_base_prime then Fp n else error ()

instance Eq Fp where
    {-# INLINEABLE (==) #-}
    Fp a == Fp b = a == b

instance AdditiveSemigroup Fp where
    {-# INLINEABLE (+) #-}
    (+) (Fp a) (Fp b) = Fp $ (a + b) `modulo` bls12_381_base_prime

instance AdditiveMonoid Fp where
    {-# INLINEABLE zero #-}
    zero = Fp 0

instance AdditiveGroup Fp where
    {-# INLINEABLE (-) #-}
    (-) (Fp a) (Fp b) = Fp $ (a - b) `modulo` bls12_381_base_prime

-- This is a more efficient way to calculate the additive inverse
-- Be sure that you are using this one instead of the one from PlutusTx.Numeric.
{-# INLINEABLE negateFp #-}
negateFp :: Fp -> Fp
negateFp (Fp x) = if x == 0 then Fp 0 else Fp $ bls12_381_base_prime - x

instance MultiplicativeSemigroup Fp where
    {-# INLINEABLE (*) #-}
    (*) (Fp a) (Fp b) = Fp $ (a * b) `modulo` bls12_381_base_prime

instance MultiplicativeMonoid Fp where
    {-# INLINEABLE one #-}
    one = Fp 1

instance Module Integer Fp where
    {-# INLINEABLE scale #-}
    scale :: Integer -> Fp -> Fp
    scale e (Fp b) = Fp $ expModInteger b e bls12_381_base_prime

-- Implementing the multiplicative group for the Fp type via extended euclidean method
instance MultiplicativeGroup Fp where
    {-# INLINEABLE recip #-}
    recip (Fp a) = Fp $ expModInteger a (-1) bls12_381_base_prime
    {-# INLINEABLE div #-}
    div a b = a * recip b

-- This is a special case of modular exponentiation, where the exponent is a power of two.
-- This saves alot of script budget. Here we mean for x^e, that e = 2^k
{-# INLINEABLE powerOfTwoExponentiationFp #-}
powerOfTwoExponentiationFp :: Fp -> Integer -> Fp
powerOfTwoExponentiationFp x k = if k < 0 then error () else go x k
  where
    go x' k' =
        if k' == 0
            then x'
            else powerOfTwoExponentiationFp (x' * x') (k' - 1)

instance Ord Fp where
    {-# INLINEABLE (<) #-}
    (<) :: Fp -> Fp -> Bool
    Fp a < Fp b = a < b
    {-# INLINEABLE (<=) #-}
    Fp a <= Fp b = a <= b
    {-# INLINEABLE (>) #-}
    Fp a > Fp b = a > b

-- {-# INLINABLE (>=) #-}
-- Fp a >= Fp b = a >= b

-- Implementing the base field Fp2

-- The field elements are the x and y coordinates of the points on the complexified curve.
data Fp2 = Fp2
    { c0 :: Fp
    , c1 :: Fp
    }
    deriving (Haskell.Show)
makeLift ''Fp2
makeIsDataIndexed ''Fp2 [('Fp2, 0)]

instance Eq Fp2 where
    {-# INLINEABLE (==) #-}
    Fp2 x1 y1 == Fp2 x2 y2 = x1 == x2 && y1 == y2

instance AdditiveSemigroup Fp2 where
    {-# INLINEABLE (+) #-}
    (+) (Fp2 a b) (Fp2 c d) = Fp2 (a + c) (b + d)

instance AdditiveMonoid Fp2 where
    {-# INLINEABLE zero #-}
    zero = Fp2 zero zero

instance AdditiveGroup Fp2 where
    {-# INLINEABLE (-) #-}
    (-) (Fp2 a b) (Fp2 c d) = Fp2 (a - c) (b - d)

-- This is a more efficient way to calculate the additive inverse
-- Be sure that you are using this one instead of the one from PlutusTx.Numeric.
{-# INLINEABLE negateFp2 #-}
negateFp2 :: Fp2 -> Fp2
negateFp2 (Fp2 a b) = Fp2 (negateFp a) (negateFp b)

-- note that we perform complex multiplication here!
instance MultiplicativeSemigroup Fp2 where
    {-# INLINEABLE (*) #-}
    (*) (Fp2 a b) (Fp2 c d) = Fp2 (a * c - b * d) (a * d + b * c)

instance MultiplicativeMonoid Fp2 where
    {-# INLINEABLE one #-}
    one = Fp2 one zero

-- Scaling a base and exponent via squaring. As an alternative, see CIP 109.
{-# INLINEABLE powModFp2 #-}
powModFp2 :: Fp2 -> Integer -> Fp2
powModFp2 b e
    | e < 0 = zero
    | e == 0 = one
    | even e = powModFp2 (b * b) (e `divide` 2)
    | otherwise = b * powModFp2 (b * b) ((e - 1) `divide` 2)

instance Module Integer Fp2 where
    {-# INLINEABLE scale #-}
    scale :: Integer -> Fp2 -> Fp2
    scale e b = powModFp2 b e

-- Implementing the multiplicative group for the Fp2 type via Fermat's little theorem
-- note that division by zero is not possible.
instance MultiplicativeGroup Fp2 where
    {-# INLINEABLE div #-}
    div a b =
        if b == zero
            then error ()
            else a * recip b
    {-# INLINEABLE recip #-}
    -- this is the complex reciprocal
    recip (Fp2 a b) = Fp2 (a `div` norm) (negateFp b `div` norm)
      where
        norm = a * a + b * b

-- This is a special case of modular exponentiation, where the exponent is a power of two.
-- This saves alot of script budget. Here we mean for x^e, that e = 2^k
{-# INLINEABLE powerOfTwoExponentiationFp2 #-}
powerOfTwoExponentiationFp2 :: Fp2 -> Integer -> Fp2
powerOfTwoExponentiationFp2 x k = if k < 0 then error () else go x k
  where
    go x' k' =
        if k' == 0
            then x'
            else powerOfTwoExponentiationFp2 (x' * x') (k' - 1)

instance Ord Fp2 where
    {-# INLINEABLE (<) #-}
    (<) :: Fp2 -> Fp2 -> Bool
    Fp2 a b < Fp2 c d = a < c || (a == c && b < d)
    {-# INLINEABLE (<=) #-}
    Fp2 a b <= Fp2 c d = a <= c && b <= d
    {-# INLINEABLE (>) #-}
    (>) :: Fp2 -> Fp2 -> Bool
    Fp2 a b > Fp2 c d = a > c || (a == c && b > d)

-- {-# INLINABLE (>=) #-}
-- Fp2 a b >= Fp2 c d =

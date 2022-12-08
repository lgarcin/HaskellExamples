{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module ChurchArithmetic where

import ChurchBoolean
  ( CB,
    churchFalse,
    churchTrue,
    unchurchBoolean,
  )

type CI = forall a. (a -> a) -> (a -> a)

churchAdd :: CI -> CI -> CI
churchAdd a b f x = a f (b f x)

churchMul :: CI -> CI -> CI
churchMul a b f = a (b f)

churchPow :: CI -> CI -> CI
churchPow a b = b a

churchSucc :: CI -> CI
churchSucc a f x = a f (f x)

churchPred :: CI -> CI
churchPred a f x = extract (a inc const)
  where
    id u = u
    extract k = k id
    inc g h = h (g f)
    const _ = x

churchInt :: Integer -> CI
churchInt 0 = \_ x -> x
churchInt n = churchSucc (churchInt (n - 1))

unchurchInteger :: CI -> Integer
unchurchInteger cn = cn (+ 1) 0

churchIsZero :: CI -> CB
churchIsZero n = n (const churchFalse) churchTrue

zero :: CI
zero f x = x

churchIsEqual :: CI -> CI -> CB
churchIsEqual a b
  | unchurchBoolean (churchIsZero a) = churchIsZero b
  | unchurchBoolean (churchIsZero b) = churchIsZero a
  | otherwise = churchIsEqual (churchPred a) (churchPred b)

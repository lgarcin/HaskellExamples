module Main

where

type Polynomial=[Float]

data Degree = MinusInfinity | Deg Integer deriving Show
data Valuation = PlusInfinity | Val Int deriving Show

degree :: Polynomial->Degree
degree [] = MinusInfinity
degree (e:q) = case degree q of
 MinusInfinity
  | e==0 -> MinusInfinity
  | otherwise -> Deg 0
 Deg d -> Deg (1+d)

valuation :: Polynomial -> Valuation
valuation [] = PlusInfinity
valuation (e:q)
 | e==0 = case valuation q of
  PlusInfinity -> PlusInfinity
  Val v        -> Val (1+v)
 | otherwise = Val 0

eval :: Polynomial -> Float -> Float
eval [] _ = 0
eval (e:q) x = e + x * eval q x

add :: Polynomial -> Polynomial -> Polynomial
add p [] = p
add [] q = q
add (e:p) (f:q) = let
  s = add p q
 in
  case s of
   [] | e+f==0 -> []
      | otherwise -> [e+f]
   (_:_) -> e+f:s


main::IO()
main=
 print
  (eval [0,2,3,0] 2)

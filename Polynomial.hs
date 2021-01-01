module Polynomial where

strip :: (Num a, Eq a) => [a] -> [a]
strip [] = []
strip l
  | last l == 0 = strip (init l)
  | otherwise = l

newtype Polynomial a
  = Polynomial [a]
  deriving (Show)

data Degree
  = MinusInfinity
  | Deg Integer
  deriving (Show)

data Valuation
  = PlusInfinity
  | Val Int
  deriving (Show)

degree :: (Num a, Eq a) => Polynomial a -> Degree
degree (Polynomial []) = MinusInfinity
degree (Polynomial (e : q)) = case degree (Polynomial q) of
  MinusInfinity
    | e == 0 -> MinusInfinity
    | otherwise -> Deg 0
  Deg d -> Deg (1 + d)

valuation :: (Num a, Eq a) => Polynomial a -> Valuation
valuation (Polynomial []) = PlusInfinity
valuation (Polynomial (e : q))
  | e == 0 = case valuation (Polynomial q) of
    PlusInfinity -> PlusInfinity
    Val v -> Val (1 + v)
  | otherwise = Val 0

eval :: (Num a) => Polynomial a -> a -> a
eval (Polynomial []) _ = 0
eval (Polynomial (e : q)) x = e + x * eval (Polynomial q) x

add :: (Num a, Eq a) => [a] -> [a] -> [a]
add p [] = p
add [] q = q
add (e : p) (f : q) = (e + f) : add p q

mul1 :: (Num a, Eq a) => a -> [a] -> [a]
mul1 0 _ = []
mul1 c l = map (c *) l

mul2 :: (Num a, Eq a) => [a] -> [a]
mul2 [] = []
mul2 l = 0 : l

mul :: (Num a, Eq a) => [a] -> [a] -> [a]
mul [] _ = []
mul (e : p1) q1 = add (mul1 e q1) (mul2 (mul p1 q1))

instance (Num a, Eq a) => Num (Polynomial a) where
  (Polynomial p) + (Polynomial q) = Polynomial . strip $ add p q
  (Polynomial p) * (Polynomial q) = Polynomial . strip $ mul p q
  (Polynomial p) - (Polynomial q) = Polynomial . strip $ add p (mul1 (-1) q)
  abs (Polynomial l) = Polynomial (map abs l)
  signum (Polynomial []) = Polynomial []
  signum (Polynomial l) = Polynomial (map signum l)
  fromInteger a = Polynomial [fromInteger a]

deriv1 :: (Num a, Eq a) => [a] -> [a]
deriv1 [] = []
deriv1 (_ : q) = add (mul2 (deriv1 q)) q

deriv :: (Num a, Eq a) => Polynomial a -> Polynomial a
deriv (Polynomial l) = Polynomial . strip . deriv1 $ l

integr1 :: (Num a, Fractional a) => [a] -> Integer -> [a]
integr1 [] _ = []
integr1 (e : q) n = (e / fromInteger (n + 1)) : integr1 q (n + 1)

integr :: (Eq a, Fractional a) => Polynomial a -> Polynomial a
integr (Polynomial l) = Polynomial . strip $ mul2 $ integr1 l 0

main :: IO ()
main =
  print $
    (Polynomial [0, 2 / 5, 3, 0] :: Polynomial Rational)
      + (Polynomial [0, 2 / 5, 3, 0] :: Polynomial Rational)

module Main where

decomp :: Integer -> Integer -> [Integer]
decomp 0 _ = []
decomp n b = rem n b : decomp (div n b) b

euclide :: Integer -> Integer -> (Integer,Integer,Integer)
euclide a 0 = if a<0 then (-a,-1,0) else (a,1,0)
euclide a b = let
  (q,r) = (div a b, rem a b)
  (d,u,v) = euclide b r
  in
  (d,v,u-q*v)

main::IO()
main=
  print (euclide 34 26)

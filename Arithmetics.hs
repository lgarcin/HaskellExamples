module Main where

decomp :: Integer -> Integer -> [Integer]
decomp 0 _ = []
decomp n b = rem n b : decomp (div n b) b

main::IO()
main=
  putStrLn "hello, world"

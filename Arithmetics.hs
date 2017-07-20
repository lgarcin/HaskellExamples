decomp n b=
  if n==0 then [] else n%b::(decomp n//b b)

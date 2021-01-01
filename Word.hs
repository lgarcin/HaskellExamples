module Word where

longestCommonPrefix :: String -> String -> String
longestCommonPrefix "" _ = ""
longestCommonPrefix _ "" = ""
longestCommonPrefix (c1 : s1) (c2 : s2) =
  if c1 == c2 then c1 : longestCommonPrefix s1 s2 else ""

distance :: String -> String -> Int
distance s1 s2 =
  length s1 + length s2 - 2 * length (longestCommonPrefix s1 s2)

longestCommonSuffix :: String -> String -> String
longestCommonSuffix s1 s2 =
  reverse $ longestCommonPrefix (reverse s1) (reverse s2)

longestCommonSubWord :: String -> String -> String
longestCommonSubWord s1 s2 = s1

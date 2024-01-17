module ChurchBoolean where

type CB = forall a. a -> a -> a

churchTrue, churchFalse :: CB
churchTrue a _ = a
churchFalse _ b = b

churchNot :: CB -> CB
churchNot m a b = m b a

churchAnd :: CB -> CB -> CB
churchAnd a b = a b a

churchOr :: CB -> CB -> CB
churchOr a b = a a b

churchXor :: CB -> CB -> CB
churchXor a b = a (churchNot b) b

churchIfThenElse :: CB -> CB -> CB -> CB
churchIfThenElse m a b = m a b

churchIff :: CB -> CB -> CB
churchIff a b = a b (churchNot b)

unchurchBoolean :: CB -> Bool
unchurchBoolean m = m True False

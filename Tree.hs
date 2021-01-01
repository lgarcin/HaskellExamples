module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

height :: Tree a -> Integer
height Nil = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)

size :: Tree a -> Integer
size Nil = 0
size (Node _ t1 t2) = 1 + size t1 + size t2

numberLeaves :: Tree a -> Integer
numberLeaves Nil = 0
numberLeaves (Node _ Nil Nil) = 1
numberLeaves (Node _ t1 t2) = numberLeaves t1 + numberLeaves t2

data Element a b = Element
  { key :: a,
    value :: b
  }
  deriving (Show)

type BinarySearchTree a b = Tree (Element a b)

search :: Ord a => a -> BinarySearchTree a b -> Maybe b
search _ Nil = Nothing
search k (Node n t1 t2)
  | k < key n = search k t1
  | k > key n = search k t2
  | otherwise = Just $ value n

insert :: Ord a => Element a b -> BinarySearchTree a b -> BinarySearchTree a b
insert elt Nil = Node elt Nil Nil
insert elt (Node n t1 t2)
  | key elt < key n = Node n (insert elt t1) t2
  | key elt > key n = Node n t1 (insert elt t2)
  | otherwise = Node elt t1 (insert n t2)

remove :: Ord a => a -> BinarySearchTree a b -> BinarySearchTree a b
remove _ Nil = Nil
remove k (Node n t1 t2)
  | k < key n = Node n (remove k t1) t2
  | k > key n = Node n t1 (remove k t2)
  | otherwise = case (t1, t2) of
    (Nil, _) -> t2
    (_, Nil) -> t1
    (_, _) -> Node m t1 t where (m, t) = removeMin t2

removeMin ::
  Ord a => BinarySearchTree a b -> (Element a b, BinarySearchTree a b)
removeMin Nil = error "Minimum not found"
removeMin (Node m Nil t) = (m, t)
removeMin (Node _ t1 t2) = (m, Node m t t2) where (m, t) = removeMin t1

mytree1 :: BinarySearchTree Integer Integer
mytree1 = Nil

mytree2 :: BinarySearchTree Integer Integer
mytree2 = insert (Element 0 45) mytree1

mytree3 :: BinarySearchTree Integer Integer
mytree3 = insert (Element 1 98) mytree2

mytree4 :: BinarySearchTree Integer Integer
mytree4 = insert (Element 2 12) mytree3

mytree5 :: BinarySearchTree Integer Integer
mytree5 = insert (Element (-1) 142) mytree4

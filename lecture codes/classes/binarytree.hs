data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a)

b = (Node 3 (Node 1 Nil (Node 2 Nil Nil))  (Node 5 (Node 4 Nil Nil) (Node 6 Nil Nil)))
d = (Node 3 (Node 1 Nil (Node 4 Nil Nil))  Nil)

spaces = (Node "Place" (Node "Home" Nil Nil) (Node "Work" Nil Nil))



-- "follow the definition" 
inOrder Nil = []
inOrder (Node n l r) = (inOrder l) ++ [n] ++ (inOrder r)

occurs k Nil = False
occurs k (Node n l r) = if n==k then True else (occurs k l) || (occurs k r)

mapTree f Nil = Nil
mapTree f (Node n l r) = (Node (f n) (mapTree f l) (mapTree f r))


-- instance Show a => Show (BinaryTree a) where
--   show Nil = ""
--   show (Node n Nil Nil) = (show n)
--   show (Node n l r) = (show n) ++ "(" ++ (show l) ++ ") (" ++ (show r) ++ ")"
  
instance Functor BinaryTree where
  fmap = mapTree

  
instance (Eq a) => Eq (BinaryTree a) where
  (==) Nil Nil = True
  (==) Nil _   = False
  (==) _ Nil   = False
  (==) (Node n1 left1 right1) (Node n2 left2 right2) = (n1==n2) && (left1==left2) && (right1==right2)
  

instance (Ord a) => Ord (BinaryTree a) where
  (<=) t1 t2 = (inOrder t1) <= (inOrder t2)


instance Show a => Show (BinaryTree a) where
  show t = prettyPrint t 0
    where
      prettyPrint Nil lvl = "+" ++ (replicate (2*lvl) '-') ++ "." 
      prettyPrint (Node n Nil Nil) lvl = "+" ++ (replicate (2*lvl) '-') ++  (show n) 
      prettyPrint (Node n l r) lvl = "+" ++ (replicate (2*lvl) '-') ++ (show n) ++ "\n"
                                                                ++ (prettyPrint l (lvl+1)) ++ "\n" 
                                                                ++ (prettyPrint r (lvl+1)) 

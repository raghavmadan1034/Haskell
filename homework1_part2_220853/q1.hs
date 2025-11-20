data Tree a = Nil | Node a (Tree a) (Tree a)

mapTree:: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

-- Identity Law 
--- Need to show that mapTree id t =id t for any tree t
--- Base Case t = Nil
---- mapTree f Nil = Nil (holds for all functions)
---- mapTree id Nil = Nil = id Nil
--- Inductive step, t= Node x left right
---- mapTree id (Node x left right) = Node (id x) (mapTree id left) (mapTree id right)
---- in the inductive hypothesis, we assume that mapTree preserves id, so mapTree id left = left, and mapTree id right = right
---- Therefore mapTree id (Node x left right)= Node x left right
---- By definition of id, id x = x
---- so RHS = id(Node x left right)= Node x left right
---- The law also holds for the inductive step

-- Composition Law
--- Need to show that mapTree (f . g) t = ((mapTree f) . (mapTree g)) t for any tree t
--- Base Case t = Nil
----- LHS -> mapTree (f . g) Nil = Nil
----- RHS -> mapTree f (mapTree g Nil) = mapTree f Nil = Nil
--- Inductive step, t = Node x left right
----- Using Induction Hypothesis we assume it holds that mapTree (f . g) left = mapTree f (mapTree g left) and mapTree (f . g) right = mapTree f (mapTree g right)
----- LHS -> mapTree (f . g) (Node x left right) = Node ((f . g) x) (mapTree (f . g) left) (mapTree (f . g) right)
----- RHS -> mapTree f (mapTree g (Node x left right)) = mapTree f (Node (g x) (mapTree g left) (mapTree g right)) = Node (f (g x)) (mapTree f (mapTree g left)) (mapTree f (mapTree g right))
----- By definition of the composition function, (f . g) x = f (g (x)), and by the induction hypothesis, LHS = RHS 

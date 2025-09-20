data NestedList a  = Nil |
  LL1 a (NestedList a) |
  LL2 (NestedList a) (NestedList a)


nlmap f Nil         = Nil
nlmap f (LL1 x ys)  = LL1 (f x) (nlmap f ys)
nlmap f (LL2 xs ys) = LL2 (nlmap f xs) (nlmap f ys)


shownl  Nil                = ""
shownl (LL1 x Nil)         = "[" ++ show x ++ "]"
shownl (LL1 x ys)          = "[" ++ (show x) ++ "," ++ (shownl ys) ++ "]"
shownl (LL2 xs (LL1 x ys)) = "[" ++ (shownl xs) ++ "," ++ (show x) ++ (shownl ys) ++ "?]" 
--shownl (LL2 xs ys)         = "[" ++ (shownl xs) ++ "," ++ (shownl ys) ++ "]"
                                         
instance Functor NestedList where
  fmap = nlmap

instance Show a => Show (NestedList a) where
  show = shownl
  

-- [1 [1] 1] is representable as
  -- LL1 1 (LL2 (LL1 1 Nil) (LL1 1 Nil)))

-- [1 1 1] is represented as
-- LL 1 (LL1 1 (LL1 1 Nil))

-- [1 1 [1]] is represented as
-- LL 1 (LL 1 (LL2 (LL1 1 Nil) Nil))

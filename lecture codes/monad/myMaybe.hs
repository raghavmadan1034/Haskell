data MyMaybe a = MyNothing | MyJust a
  deriving Show
 
instance Eq a => Eq (MyMaybe a) where
  (==) MyNothing MyNothing    = True
  (==) MyNothing _          =  False
  (==) _       MyNothing    =  False
  (==) (MyJust x) (MyJust y)  = x==y

-- =============================================================
--   Functor
-- =============================================================
instance Functor MyMaybe where
  fmap = mapMyMaybe where
    mapMyMaybe f MyNothing = MyNothing
    mapMyMaybe f (MyJust x) = MyJust (f x)


-- =============================================================
--   Applicative
-- =============================================================


instance Applicative MyMaybe where
  pure = MyJust
  (MyJust foo) <*> mx = fmap foo mx
  MyNothing <*> mx = MyNothing      -- line not mentioned in Hutton
-- or use the infix notation for fmap : foo <$> mx



-- Usage: (+) <$> (MyJust 1) <*> (MyJust 3)
-- Another example:
-- ================
-- max3 x y z = max  (max x y) z
-- Usage: max3 <$> (MyJust 1) <*> (MyJust 2) <*> (MyJust 3)
-- Another usage
-- =============
-- (pure max3) <*> (MyJust 1) <*> (MyJust 2) <*> (MyJust 3)



-- ============================================================
--   Monad 
-- ============================================================

instance Monad MyMaybe where
  return = pure
  MyNothing >>= f = MyNothing
  (MyJust x) >>= f = f x      


-- ========================================
--  Examples
-- ========================================
-- safe reciprocal
safeReciprocal 0 = MyNothing
safeReciprocal x = MyJust (1/x)

-- Example usage
--  (MyJust 0.5) >>= safeReciprocal >>= safeReciprocal
--   (MyJust 0) >>= safeReciprocal >>= safeReciprocal

-- safe integer division
safeDiv m 0 = MyNothing
safeDiv m n = MyJust (m `div` n)

-- Example usage
-- (MyJust 4) >>= (\m ->
--         (MyJust 2) >>= (\n ->
--                safeDiv m n))

-- using do notation
--   do
--     m <- myJust 4
--     n <- myJust 2
--     safeDiv m n

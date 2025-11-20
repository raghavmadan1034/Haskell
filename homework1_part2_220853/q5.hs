data Expr a= Var a| Val Int | Add (Expr a) (Expr a)

instance Functor Expr where
    fmap f (Var x)= Var (f x)
    fmap f (Val a)= Val a
    fmap f (Add x1 x2)= Add (fmap f x1) (fmap f x2)

instance Applicative Expr where
    pure = Var
    (Var f) <*> e =fmap f e
    (Val a) <*> _ = Val a
    (Add f g) <*> e= Add (f <*> e) (g <*> e)

instance Monad Expr where 
    return = pure
    (Var x) >>= f = f x
    (Val a) >>= f = Val a
    (Add x1 x2) >>= f = Add (x1 >>= f) (x2 >>= f)

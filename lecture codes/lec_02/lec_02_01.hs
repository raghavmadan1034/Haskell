fact 0 = 1
fact n = n * (fact (n-1))



-- A collection of higher order functions
-- Many of them are provided in the Prelude
-- But, we will try to code them up in our own way

map f [] = []
map f (x:xs) = (f x) : (Main.map f xs)

foldLeft binOp start [] = start
foldLeft binOp start (x:xs) = foldLeft binOp (binOp start x) xs

square n = n*n
cube n   = n*n*n


filter predicate [] = []
filter predicate (x:xs) = if (predicate x)
                          then x:(Main.filter predicate xs)
                          else (Main.filter predicate xs)


filter2 predicate [] = []
filter2 predicate (x:xs) = let rest = (filter2 predicate xs) in
                             if (predicate x)
                             then x:rest
                             else rest
                             
neg True = False
neg False = True

isEven n = (mod n 2)==0


zipWith f [] [] = []
zipWith f (x:xs) (y:ys) = (f x y):(Main.zipWith f xs ys)

argPair f lst = Main.zipWith (,) lst (Main.map f lst)

-- just for fun

apply_multiple fun init_arg 0 = init_arg
apply_multiple fun init_arg n = fun $ apply_multiple fun init_arg $n-1

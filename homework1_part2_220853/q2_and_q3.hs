-- Question 2 and Question 3 combined solution
data NestedList a= Elem a|List[NestedList a]

joinWith :: String -> [String] -> String
joinWith _   []     = ""
joinWith _   [s]    = s
joinWith sep (s:ss) = s ++ sep ++ joinWith sep ss

instance Show a => Show (NestedList a) where
    show(Elem x)=show x
    show (List xs) = "[" ++ joinWith ", " (map show xs) ++ "]"
instance Functor NestedList where
    fmap f (Elem x)= Elem (f x)
    fmap f (List xs)= List(map (fmap f) xs)

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

main :: IO ()
main = do

    let list1 = List [ List [Elem 1] ]
    let list2 = List [ List [Elem 1], Elem 1, List [Elem 1] ] --[[1],1,[1]]
    let list3 = List [ List [ List [Elem 1] ], Elem 1]  --[[[1]],1]
    print list1
    print list2
    print list3
    print (flatten list1)
    print (flatten list2)
    print (flatten list3)



recur i xs=
    if i == min (length(xs)-1) (n-1)
    then 1 `div` xs!!i 
    else 1 `div` (add (xs!!i) (recur (i+1) xs))
answer xs= recur 0 xs

freeVar (Var x) =[x]
freeVar (Term (LamdaTerm y lam) (LambdaTerm x lam))=[freeVar(LamdaTerm y lam)] ++ [freeVar(LamdaTerm x lam)]
freeVar (Prod (lam k) (Lambda x lam))= filtr f [freeVar (Lambda x lam)] where f= (x!=k)
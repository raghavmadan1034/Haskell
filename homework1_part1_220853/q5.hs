newmap f []=[]
newmap f x=foldr (\x acc -> (f x):acc) [] x


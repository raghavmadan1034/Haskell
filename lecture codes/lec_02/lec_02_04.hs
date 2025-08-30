
fst [] =  undefined -- error "Empty List"
fst (x:xs) = x

snd [] = []
snd (x:xs) = xs


-- take

take n [] = []
take 0 _  = []
take n (x:xs) = x:(Main.take (n-1) xs)

-- drop
drop n [] = []
drop 0 (x:xs) = (x:xs)
drop n (x:xs) = Main.drop (n-1) xs

-- Tail recursion


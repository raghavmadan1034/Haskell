neg True       = False
neg False      = True

id a = a


-- Pattern matching!
-- Try the type of this function
or _ True      = True
or True _      = True
or _ _ = False




-- instead of pattern matching, we can use "guards" - 
-- i.e. testing for conditions
isPositive n | n<=0 = False
             | otherwise = True


-- Polymorphic length
-- note the syntax for pattern matching on lists (first element : rest of the list)
len [] = 0
len (x:xr) = 1+ (len xr)

-- reverse a list
rev [] = []
rev (x:xr) = (rev xr)++[x]

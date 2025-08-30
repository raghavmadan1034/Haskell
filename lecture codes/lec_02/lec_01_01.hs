neg True       = False
neg False      = True

id a = a


-- Pattern matching!
-- Try the type of this function

or _ True      = True
or True _      = True
or _ _ = False



-- Polymorphic length
len [] = 0
len (x:xs) = 1+ (len xs)

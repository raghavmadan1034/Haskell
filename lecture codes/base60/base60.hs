-- Ancient Babylonians used a base 60 number system.
-- One justification is that 60 is the smallest number <= 100
-- which has the largest number of factors. This makes it easy
-- to divide it into administrative divisions in the most 
-- flexible manner possible. We verify this claim using the
-- code below. 

-- Note the use of the higher-order functions map, zip, foldr
-- Especially note the definitions of max* functions, which 
-- use foldr to avoid the traditional loop to find the maximum
-- of a list

factor n d             = mod n d == 0

factors n              = filter (\ d -> factor n d) [1..n]

listOfFactorsUpTo n    = map factors [1..n]

numFactorsUpTo n       = map length (listOfFactorsUpTo n)

numAndFactorsUpTo n    = zip [1..n] (numFactorsUpTo n)

maxList xs             = foldr max (-1) xs

tupleMax t1 t2         = if ((snd t1)>=(snd t2)) then t1 else t2

maxNumAndFactorsUpTo n = foldr tupleMax (0,-1) (numAndFactorsUpTo n)

isNonMultiple n x = (mod n x) /= 0

nonMultiples k xs = filter (\ n -> isNonMultiple n k) xs

sieve  []	  = []
sieve  (x:xr)	  = x:(sieve (nonMultiples x xr))


-- streamstyle

addStreams xs ys     = zipWith (+) xs ys
productStreams xs ys = zipWith (*) xs ys

ones = 1:ones
ints = 1:(addStreams ones ints)

intsFrom2 = tail ints



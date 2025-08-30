filter pred [] = []
filter pred (x:xr) = if (pred x) then (x:(Main.filter pred xr)) else (Main.filter pred xr)

base = 2

-- Extract the digits from a "real" between 0 and 1
-- note that in base 10, multiplying by 10 shifts the number's base 10
-- representation left by one digit.
-- similarly for any base
fractionalPart r        = r - fromIntegral (floor r)
firstDigit r            = floor (base*r)
digits r                = (firstDigit r) : digits (fractionalPart (base*r))

-- xs[s..(e-1)] : returns e-s elements if avaialable
slice s e xs            =  drop s (take e xs)

slices xs j             = [ slice (k*j) ((k+1)*j) xs |
                            k <- [0..(div (Main.length xs) j)-1] ]

-- check whether all slices of xs of length j are the same
isRecurrenceLength xs j = let ys = slices xs j in all (== (head ys)) ys 

-- find minimum integer greater than 1 for which predictate is true
findMin pred            = findMinFrom 1 pred
  where
    findMinFrom n pred  = if (pred n) then n else (findMinFrom (n+1) pred)

-- find the repeating length of xs. Note: passes a partially evaluated function
findRecurrenceLength xs = findMin (isRecurrenceLength xs)

-- find if d has a repeating block length of length d-1
isBadDenominator d = (d-1)==
                     let
                       digitList     = digits (1/(fromIntegral d))
                       len           = 2*d
                       truncatedList = take len digitList
                     in
                       findRecurrenceLength truncatedList

  
-- idea : list all bad denominators
-- idea : how to change base

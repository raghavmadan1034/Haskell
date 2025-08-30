succ 0 = 1
succ n = 1+(Main.succ (n-1))

isEven n = (mod n 2) == 0

hailstone 1 = 1
hailstone n = if (isEven n) then (hailstone (div n 2)) else (hailstone (3*n+1))


-- to do: compute the number of iterations before the hailstone reaches 1

                              

-- hailstoneCount n = fst (hailstoneAux n 0)
--   where
--     hailstoneAux 1 count = (count,1)
--     hailstoneAux n count = if (isEven n)
--                          then  (hailstoneAux (div n 2) (count+1))
--                          else  (hailstoneAux (3*n+1)   (count+1))



maxUpTo f 0 = 0
maxUpTo f n = max (f n) (maxUpTo f (n-1))

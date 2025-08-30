fibM 0 _ = (1,[1])
fibM 1 _ = (1,[1,1])
fibM n table =
  if (length table)>n
  then (table !! (n), table)
  else
    let
      (v1,t1) = fibM (n-2) table
      (v2,t2) = fibM (n-1) t1 
      (v3,t3) = (v1+v2, t2++[v3])
    in
      (v3,t3)

      

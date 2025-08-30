succ 0 = 1
succ n = 1+(Main.succ (n-1))

-- a first example of a higher-order function
max_up_to f 0 = 0
max_up_to f n = max (f n) (max_up_to f (n-1))

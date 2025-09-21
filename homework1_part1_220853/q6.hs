encoding []=[]
encoding (x:xs)=aux x 1 xs
    where 
        aux curr cnt []=[(curr,cnt)]
        aux curr cnt (y:ys)
            |y==curr =aux curr (cnt+1) ys
            |otherwise =(curr,cnt): aux y 1 ys

main:: IO()
main = do
    let list="ATTCGCCGG"
    let newlist = encoding list
    print newlist
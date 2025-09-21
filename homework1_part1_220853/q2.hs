func n []=[]
func n x=aux n x 1 []
    where aux n x idx acc
            | n*idx>(length x) =acc
            | otherwise = aux n x (idx+1) (acc++ [x!!(idx*n-1)])

main:: IO()
main = do
    let list=[1,2,3,4,5,6,7,8,9,10,11,12,13]
    let newlist = func 3 list
    print newlist
reverselist []=[]
reverselist x=aux x []
    where aux x acc
            |x==[] =acc
            |otherwise =aux (tail x) ((head x):acc)

main:: IO()
main = do
    let list=[1,2,3,4]
    let newlist = reverselist list
    print newlist
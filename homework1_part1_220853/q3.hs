insert x []=[[x]]
insert x (y:ys)=(x:y:ys):[y:zs|zs<- insert x ys]

permutation []=[[]]
permutation (x:xs)=concatMap (insert x) (permutation xs)

main:: IO()
main = do
    let list=[1,2,3]
    let newlist = permutation list
    print newlist
returnlast []=0
returnlast (x:xs)=if(xs==[]) then x else (returnlast xs)

main:: IO()
main = do
    let list=[1,2,3,4,5]
    let lastElement = returnlast list
    print lastElement
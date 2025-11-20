safeHead []= Nothing
safeHead xs= Just(head xs)

safeAdd x y= Just(x+y)

result xs=safeHead xs >>=(\n->safeAdd n 3)
result_new xs= do
    n <- safeHead xs
    safeAdd n 3

main :: IO ()
main = do
    let x=[1,3,3]
    print(safeHead x)
    print(result x)
    print(result_new x)

    
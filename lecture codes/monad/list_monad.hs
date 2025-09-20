inc x = [succ x]

main =
  putStrLn $ show $ [1,2,3] >>= inc >>= inc

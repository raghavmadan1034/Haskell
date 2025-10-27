import Control.Monad (forM)

processIndex i = do
  putStrLn ("Enter message " ++ show i ++ ":")
  getLine

main = do
  messages <- forM [1, 2, 3] processIndex
  putStrLn (show messages)

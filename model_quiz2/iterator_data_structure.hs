-- use State monad to implement a list
-- which has an internal pointer to the next
-- element to be printed.

import Control.Monad.State

-- There is a list of type [a], and an int
-- denoting the current position in the list
data Iterator a  = Iterator [a] Int
  deriving (Show, Eq)

iterFromList xs = Iterator xs 0

iterStart :: State (Iterator a)  ()
iterStart = state $ (\(Iterator xs _)->((), Iterator xs 0))

iterCurrent :: State (Iterator a) a
iterCurrent = state $ (\(Iterator xs p)->(xs!!p, (Iterator xs p)))

iterNext :: State (Iterator a) ()
iterNext = state $ (\(Iterator xs p) -> ((), (Iterator xs (safeIncrement xs p))))

safeIncrement xs k = min (k+1) ((length xs)-1)
                    

main :: IO ()
main = do
  let itr = iterFromList [10,20,30]
      (results, finalItr) = runState (do
        c1 <- iterCurrent
        iterNext
        c2 <- iterCurrent
        iterNext
        c3 <- iterCurrent
        iterNext
        c4 <- iterCurrent
        return [c1,c2,c3,c4]) itr
  putStrLn $ "Sequence of results: " ++ show results
  putStrLn $ "Final iterator state: " ++ show finalItr


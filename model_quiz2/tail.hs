-- implements a simple version "tail" command line
-- prints the last 10 lines of a file, if the file
-- has at least 10 lines. Otherwise, it prints the
-- whole file.

import System.Environment ( getArgs )
import System.Exit ( die )

tail10 :: FilePath -> IO ()
tail10 path = do
  contents <- readFile path  -- readFile
  putStr $ unlines $ takeLast10 10 $lines contents

takeLast10 n xs
  | n <= 0    = []
  | otherwise = drop ((length xs)-k) xs
                where k = min n (length xs)

main = do
  args <- getArgs -- getArgs
  case args of
    [file] -> tail10 file
    _      -> die "Usage"
    

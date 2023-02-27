module Caesar where

import Data.Char

cleanUp '\n' = [' ']
cleanUp '\t' = [' ']
cleanUp ' ' = [' ']
cleanUp c = [toLower c | isAlpha (toLower c)]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
 | isLower c = int2let ((let2int c + n) `mod` 26)
 | otherwise = c

main = do
  raw <- fmap (concatMap cleanUp) (readFile "./src/alice.txt")
  writeFile "./src/code.txt" (map (shift 7)  raw)

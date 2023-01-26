module Day1 where

import Data.List (elemIndex)

parenValue :: Char -> Integer
parenValue '(' = 1
parenValue ')' = -1
parenValue _ = 0

partOne :: String -> Integer
partOne = sum . fmap parenValue

partTwo :: String -> Maybe Int
partTwo = elemIndex (-1) . scanl (+) 0 . map parenValue

main :: IO ()
main = do
  contents <- readFile "./src/Advent/2015/Day1.txt"
  print (partOne contents)
  print (partTwo contents)

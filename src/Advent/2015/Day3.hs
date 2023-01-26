module Day3 where

import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

move :: Point -> Char -> Point
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move (x, y) '^' = (x, y - 1)
move (x, y) 'v' = (x, y + 1)
move p _ = p

visited :: [Char] -> Set Point
visited dirs = Set.fromList $ scanl move (0, 0) dirs

partOne :: [Char] -> Int
partOne dirs = Set.size (visited dirs)

partTwo :: [Char] -> Int
partTwo dirs =
  let santaDirs = snd <$> filter (even . fst) (zip [(0::Int)..] dirs)
      robotDirs = snd <$> filter (odd  . fst) (zip [(0::Int)..] dirs)
  in Set.size (visited santaDirs <> visited robotDirs)

main :: IO ()
main = do
  contents <- readFile "./src/Advent/2015/Day3.txt"
  print $ partOne contents
  print $ partTwo contents

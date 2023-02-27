{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T

parse :: Text -> (Int, Int, Int)
parse s =
  case fmap T.unpack (T.splitOn "x" s) of
    [a,b,c] -> (read a, read b, read c)
    x -> error ("Failed to parse " ++ show x)

area :: (Int, Int, Int) -> Int
area (x, y, z) =
  let a = y * z
      b = x * z
      c = x * y
  in a * 2 + b * 2 + c * 2 + minimum [a, b, c]

ribbon :: (Int, Int, Int) -> Int
ribbon (x, y, z) =
  let [a, b, _] = sort [x, y, z]
  in a * 2 + b * 2 + x * y * z

main :: IO ()
main = do
  contents <- fmap T.pack (readFile "./src/Advent/2015/Day2.txt")
  let dimensions = fmap parse (T.lines contents)
  print (sum (fmap area dimensions))
  print (sum (fmap ribbon dimensions))

module Monads where

import Data.Maybe (fromJust)

-- Part 1: We want to write functions on integers, and handle NaN correctly throughout the calculations

data N = Nan | Number Int
  deriving Show

half :: N -> N
half Nan = Nan
half (Number x) = if even x then Number (x `div` 2) else Nan

ex0 :: N
ex0 = half (Number 10)

ex1 :: N
ex1 = half (half (Number 10))

double :: N -> N
double Nan = Nan
double (Number x) = Number (x *  2)

-- Gets tedious to write these functions, we want to be able to use
-- normal math functions and just "wrap" them in this logic, right?

pipe :: N -> (Int -> N) -> N
pipe Nan _ = Nan
pipe (Number n) f = f n

ex2 :: N
ex2 = Number 9 `pipe` (\x -> if even x then Number (x `div` 2) else Nan) `pipe` (\x -> Number (x * 2))



--- Part 2: We want to create a database, but using only *pure* functions

data DB a = DB [(String, a)]
  deriving Show

readDB :: String -> DB a -> (DB a, a)
readDB key (DB bindings) = (DB bindings, fromJust (lookup key bindings))

writeDB :: String -> a -> DB a -> (DB a, ())
writeDB key value (DB bindings) = (DB ((key, value) : bindings), ())

ex3 :: Int
ex3 = let db0 = DB []
          (db1, _) = writeDB "erik" 100 db0
          (db2, _) = writeDB "johannes" 200 db1
          (_, x)   = readDB "erik" db2
      in x

ex4 :: DB Int
ex4 = let db0 = DB []
          (db1, _) = writeDB "erik" 100 db0
          (db2, x) = readDB "erik" db1
          (db3, _) = writeDB "johannes" (x * 2) db2
      in db3

-- Would like to write something like:
-- readDB "erik" (writeDB "johannes" 200 (writeDB "erik" 100 (DB [])))
-- Or rather
-- writeDB "erik" 100 ==> writeDB "johannes" 200 ==> readDB "erik"

andThen :: (DB Int, a) -> (a -> DB Int -> (DB Int, b)) -> (DB Int, b)
andThen (DB db, val) f =
  let (DB db2, res2) = f val (DB db)
  in (DB (db2 ++ db), res2)

ex5 :: (DB Int, ())
ex5 = (DB [], ())
  `andThen` \_ _ -> (DB [("erik", 100)], ())
  `andThen` \_ (DB db) -> (DB [], fromJust (lookup "erik" db))
  `andThen` \x _ -> (DB [("johannes", x * 2)], ())

-- Let's add some helpers
dbWrite :: String -> Int -> DB Int -> (DB Int, ())
dbWrite key val = \_ -> (DB [(key, val)], ())

dbRead :: String -> DB Int -> (DB Int, Int)
dbRead key = \(DB db) -> (DB [], fromJust (lookup key db))

ex6 :: (DB Int, ())
ex6 = (DB [], 0)
  `andThen` (\_ -> dbWrite "erik" 100)
  `andThen` (\_ -> dbRead "erik")
  `andThen` (\x -> dbWrite "johannes" (x * 2))

-- ex7 = runDB (DB []) (do dbWrite "erik" 100
--                         x <- dbRead "erik"
--                         dbWrite "johannes" (x * 2))


-- Part 3: The monad typeclass

-- Monad is just an "interface" (or typeclass / trait)
-- Tries to abstract this function 'pipe', 'andThen', etc.
-- Also gives us do-notation.

-- "A monad" is an instance of this interface -- that's it!

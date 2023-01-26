module FoldCompose where

-- fold can be seen as inserting an operator between each term, eg
-- foldl (+) 0 [1..5] = 0 + 1 + 2 + 3 + 4 + 5

-- If we fold function composition (dot) we get a new (composed) function!

f :: Num a => a -> a
f = foldl (.) id [(+1), (*2), (+3)]

x :: Integer
x = f 10 -- (+1) . (*2) . (+3) $ 10 = ((10 + 3) * 2) + 1 = 27

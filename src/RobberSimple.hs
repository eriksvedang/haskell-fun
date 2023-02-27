module Main where

expand :: Char -> String
expand c = if isConsonant c then [c, 'o', c] else [c]

isConsonant :: Char -> Bool
isConsonant c = elem c "bcdfghjklmnpqrstuvwxz"

robber :: String -> String
robber word = concatMap expand word

main = interact robber

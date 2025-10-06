module Main where

expand :: Char -> String
expand c = if isConsonant c then [c, 'o', c] else [c]

isConsonant :: Char -> Bool
isConsonant c = c `elem` "bcdfghjklmnpqrstuvwxz"

robber :: String -> String
robber = concatMap expand

main = interact robber

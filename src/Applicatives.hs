module Applicatives where

-- >>> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

noMonads :: IO String
noMonads = pure (<>) <*> getLine <*> getLine

noMonadsAgain :: IO [Char]
noMonadsAgain = pure reverse <*> getLine

alsoNoMonads :: [Integer]
alsoNoMonads = pure (*) <*> [1, 2, 3] <*> [10, 1000]

sameButDifferent :: [Int]
sameButDifferent = (*) <$> [1::Int, 2, 3] <*> [10, 1000]

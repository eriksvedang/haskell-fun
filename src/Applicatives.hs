module Applicatives where

-- >>> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

noMonads :: IO String
noMonads = (<>) <$> getLine <*> getLine

noMonadsAgain :: IO [Char]
noMonadsAgain = reverse <$> getLine

alsoNoMonads :: [Integer]
alsoNoMonads = (*) <$> [1, 2, 3] <*> [10, 1000]

sameButDifferent :: [Int]
sameButDifferent = (*) <$> [1 :: Int, 2, 3] <*> [10, 1000]

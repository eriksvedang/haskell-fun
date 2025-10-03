module Applicatives where

-- >>> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

noMonads = pure (<>) <*> getLine <*> getLine
noMonadsAgain = pure reverse <*> getLine

alsoNoMonads = pure (*) <*> [1, 2, 3] <*> [10, 1000]
sameButDifferent = (*) <$> [1::Int, 2, 3] <*> [10, 1000]

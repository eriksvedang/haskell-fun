module Applicatives where

-- >>> :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

noMonads = pure (<>) <*> getLine <*> getLine

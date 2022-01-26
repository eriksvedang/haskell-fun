rob :: IO ()
rob = getLine >>= putStrLn . concatMap (\c -> if c `elem` "qwrtypsdfghjklzxcvbnm" then [c,'o',c] else [c])

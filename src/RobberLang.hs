module RobberLang where

import Data.Char (toLower)

translate :: String -> String
translate = concatMap expand
  where expand c =
          if toLower c `elem` "qwrtypsdfghjklzxcvbnm"
          then [c,'o', toLower c]
          else [c]

main :: IO ()
main = interact translate

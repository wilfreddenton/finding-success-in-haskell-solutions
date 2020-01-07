module Main where

import Data.Char (isAlphaNum, isSpace)

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  if (&&) <$> (10 <=) <*> (20 >=) $ length password
    then Just password
    else Nothing

-- this function returns `Just ""` when passed `""`
-- because `all` defaults to True when the list is empty
requireAlphaNum :: String -> Maybe String
requireAlphaNum password =
  if all isAlphaNum password then Just password else Nothing

cleanWhitespace :: String -> Maybe String
cleanWhitespace ys@(x : xs) =
  if isSpace x then cleanWhitespace xs else Just ys

validatePassword :: String -> Maybe String
validatePassword password = checkPasswordLength =<< requireAlphaNum =<< cleanWhitespace password

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print $ validatePassword password

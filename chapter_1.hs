-- exercise 1
absVal :: (Num a, Ord a) => a -> a
absVal x = case (x < 0) of
  True -> negate x
  False -> x

-- exercise 2
validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password = case (null username, null password) of
  (True, True) -> "Empty username and password"
  (True, False) -> "Empty username"
  (False, True) -> "Empty password"
  (False, False) -> "Okay"

-- exercise 3
{-
safeHead :: [a] -> a
safeHead [] = []
safeHead (x : xs) = x

The above safeHead function does not work because [] is not of type a
-}

-- exercise 4
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

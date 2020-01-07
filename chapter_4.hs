-- exercise 10
desugar :: IO ()
desugar = do
  s <- getLine
  print $ reverse s

-- exercise 11
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just a) f = f a

-- exercise 12
data StringOrValue a = Str String | Val a deriving (Show)

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Val a) f = f a
bindStringOrValue (Str s) _ = Str s

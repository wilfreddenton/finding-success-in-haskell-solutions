import Data.Char (isLetter, toLower)

-- exercise 5
palindrome :: IO ()
palindrome = do
  putStr "Please enter a phrase: "
  phrase <- getLine
  print $ checkPalindrome phrase
  where
    notPalindrome = "This phrase is not a palindrome"
    palindrome = "This phrase is a palindrome."
    checkPalindrome [] = notPalindrome
    checkPalindrome phrase =
      let simplifiedPhrase = toLower <$> filter isLetter phrase
       in if simplifiedPhrase == reverse simplifiedPhrase
            then palindrome
            else notPalindrome

-- exercise 6
leetspeak :: IO ()
leetspeak = do
  putStr "Please enter a leetspeak phrase: "
  phrase <- getLine
  print $ fmap substituteChar phrase
  where
    substituteChar c =
      case c of
        'e' -> '3'
        'l' -> '1'
        'o' -> '0'
        _ -> c

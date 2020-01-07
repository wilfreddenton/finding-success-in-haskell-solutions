module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAlphaNum, isSpace)
import Data.List

newtype Username = Username String deriving (Eq, Show)

newtype Password = Password String deriving (Eq, Show)

newtype Error = Error String deriving (Eq, Show)

checkLength :: Int -> Int -> String -> Either Error String
checkLength lo hi xs =
  if (&&) <$> (lo <=) <*> (hi >=) $ length xs
    then Right xs
    else Left $ Error ("Input must be at least " <> show lo <> " and at most " <> show hi <> " characters long.")

requireAlphaNum :: String -> Either Error String
requireAlphaNum xs =
  if all isAlphaNum xs
    then Right xs
    else Left $ Error "Cannot contain whitespace or special characters."

cleanWhitespace :: String -> Either Error String
cleanWhitespace "" = Left $ Error "Cannot be empty."
cleanWhitespace ys@(x : xs) =
  if isSpace x then cleanWhitespace xs else Right ys

validatePassword :: Password -> Either Error Password
validatePassword (Password p) = Password <$> (checkLength 10 20 =<< requireAlphaNum =<< cleanWhitespace p)

validateUsername :: Username -> Either Error Username
validateUsername (Username u) = Username <$> (checkLength 3 15 =<< requireAlphaNum =<< cleanWhitespace u)

data User = User Username Password deriving (Show)

makeUser :: Username -> Password -> Either Error User
makeUser username password = User <$> validateUsername username <*> validatePassword password

-- exercise 21
makeUserTmpPassword :: Username -> Either Error User
makeUserTmpPassword username = User <$> validateUsername username <*> pure (Password "temporaryPassword")

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username <$> getLine
  putStr "Please enter a password: "
  password <- Password <$> getLine
  print $ makeUser username password

type Test e s a = ExceptT e (State s) a

runTests :: Test Error Int () -> (Either Error (), Int)
runTests = flip runState 1 . runExceptT

assertEq :: (Eq a, Show a) => a -> a -> Test Error Int ()
assertEq actual expected = do
  n <- get
  if actual /= expected
    then throwError $ Error ("Test " <> show n <> ", Expected: " <> show expected <> ", But got: " <> show actual)
    else do
      put (n + 1)
      return ()

printTestResult :: (Either Error (), Int) -> IO ()
printTestResult (Left err, _) = putStrLn $ show err
printTestResult (Right _, n) = putStrLn ("All " <> show (n - 1) <> " tests passed.")

test :: IO ()
test = printTestResult . runTests $ do
  assertEq (checkLength' "") $ Left passwordLengthErr
  assertEq (checkLength' . take 9 $ repeat '1') $ Left passwordLengthErr
  let s10 = take 10 $ repeat '1'
  assertEq (checkLength' s10) $ Right s10
  assertEq (checkLength' (s10 <> "1")) $ Right (s10 <> "1")
  let s20 = s10 <> s10
  assertEq (checkLength' s20) $ Right s20
  assertEq (checkLength' (s20 <> "1")) $ Left passwordLengthErr
  assertEq (requireAlphaNum "") $ Right ""
  assertEq (requireAlphaNum "Abc") $ Right "Abc"
  assertEq (requireAlphaNum "''") $ Left alphaNumErr
  assertEq (requireAlphaNum "a_b-c") $ Left alphaNumErr
  assertEq (cleanWhitespace " ") $ Left emptyErr
  assertEq (cleanWhitespace "  a") $ Right "a"
  assertEq (validatePassword $ Password "  asdfasdfasdf") . Right $ Password "asdfasdfasdf"
  assertEq (validatePassword $ Password "  asdfasdf") $ Left passwordLengthErr
  assertEq (validatePassword $ Password ",,,,,,,,,,,") $ Left alphaNumErr
  assertEq (validateUsername $ Username "  asdfasdf") . Right $ Username "asdfasdf"
  assertEq (validateUsername $ Username "  as") $ Left usernameLengthErr
  assertEq (validateUsername $ Username ",,,,,,,,,,,") $ Left alphaNumErr
  where
    checkLength' = checkLength 10 20
    passwordLengthErr = Error "Input must be at least 10 and at most 20 characters long."
    usernameLengthErr = Error "Input must be at least 3 and at most 15 characters long."
    alphaNumErr = Error "Cannot contain whitespace or special characters."
    emptyErr = Error "Cannot be empty."

-- exercise 22
pureMaybe :: a -> Maybe a
pureMaybe = Just

pureEither :: a -> Either l a
pureEither = Right

-- exercise 23
applicativeIO :: IO ()
applicativeIO = do
  result <- checkAnagram <$> promptWord1 <*> promptWord2
  print result
  where
    checkAnagram xs ys = sort xs == sort ys
    promptWord1 = putStr "Please enter a word: " >> getLine
    promptWord2 = putStr "Please enter a second word: " >> getLine

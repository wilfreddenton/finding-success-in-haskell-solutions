module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Char (isAlphaNum, isSpace)

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  if (&&) <$> (10 <=) <*> (20 >=) $ length password
    then Right password
    else Left "Your password must be at least 10 and at most 20 characters long."

requireAlphaNum :: String -> Either String String
requireAlphaNum password =
  if all isAlphaNum password
    then Right password
    else Left "Your password must only contain alphanumeric characters."

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Right ""
cleanWhitespace ys@(x : xs) =
  if isSpace x then cleanWhitespace xs else Right ys

validatePassword :: String -> Either String String
validatePassword password = checkPasswordLength =<< requireAlphaNum =<< cleanWhitespace password

main :: IO ()
main = do
  putStr "Please enter a password: "
  password <- getLine
  print $ validatePassword password

type Test e s a = ExceptT e (State s) a

runTests :: Test String Int () -> (Either String (), Int)
runTests = flip runState 1 . runExceptT

assertEq :: (Eq a, Show a) => a -> a -> Test String Int ()
assertEq actual expected = do
  n <- get
  if actual /= expected
    then throwError ("Test " <> show n <> ", Expected: " <> show expected <> ", But got: " <> show actual)
    else do
      put (n + 1)
      return ()

printTestResult :: (Either String (), Int) -> IO ()
printTestResult (Left err, _) = putStrLn err
printTestResult (Right _, n) = putStrLn ("All " <> show (n - 1) <> " tests passed.")

test :: IO ()
test = printTestResult . runTests $ do
  assertEq (checkPasswordLength "") $ Left lengthErr
  assertEq (checkPasswordLength . take 9 $ repeat '1') $ Left lengthErr
  let s10 = take 10 $ repeat '1'
  assertEq (checkPasswordLength s10) $ Right s10
  assertEq (checkPasswordLength (s10 <> "1")) $ Right (s10 <> "1")
  let s20 = s10 <> s10
  assertEq (checkPasswordLength s20) $ Right s20
  assertEq (checkPasswordLength (s20 <> "1")) $ Left lengthErr
  assertEq (requireAlphaNum "") $ Right ""
  assertEq (requireAlphaNum "Abc") $ Right "Abc"
  assertEq (requireAlphaNum "''") $ Left alphaNumErr
  assertEq (requireAlphaNum "a_b-c") $ Left alphaNumErr
  assertEq (cleanWhitespace " ") $ Right ""
  assertEq (cleanWhitespace "  a") $ Right "a"
  assertEq (validatePassword "  asdfasdfasdf") $ Right "asdfasdfasdf"
  assertEq (validatePassword "  asdfasdf") $ Left lengthErr
  assertEq (validatePassword ",,,,,,,,,,,") $ Left alphaNumErr
  where
    lengthErr = "Your password must be at least 10 and at most 20 characters long."
    alphaNumErr = "Your password must only contain alphanumeric characters."
-- exercise 16
{-
1. String: no
2. []: yes
3. (,): yes
4. (,) Int: yes
5. data Pair a = Pair a a: yes
-}

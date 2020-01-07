{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Validation

newtype Username = Username Text deriving (Eq, Show)

newtype Password = Password Text deriving (Eq, Show)

newtype Error = Error [Text] deriving (Eq, Show, Semigroup)

showT :: (Show a) => a -> Text
showT = T.pack . show

checkLength :: Int -> Int -> Text -> Validation Error Text
checkLength lo hi t =
  if (&&) <$> (lo <=) <*> (hi >=) $ T.length t
    then Success t
    else Failure $ Error ["Input must be at least " <> showT lo <> " and at most " <> showT hi <> " characters long."]

requireAlphaNum :: Text -> Validation Error Text
requireAlphaNum t =
  if T.all isAlphaNum t
    then Success t
    else Failure $ Error ["Cannot contain whitespace or special characters."]

validatePassword :: Password -> Validation Error Password
validatePassword (Password p) = Password <$> checkLength 10 20 p <* requireAlphaNum p

validateUsername :: Username -> Validation Error Username
validateUsername (Username u) = Username <$> checkLength 3 15 u <* requireAlphaNum u

data User = User Username Password deriving (Show)

-- exercise 26
makeUser :: Username -> Password -> Validation Error User
makeUser username password = do
  username' <- validateUsername username
  password' <- validatePassword password
  pure $ User username' password'

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username . T.strip <$> T.getLine
  putStr "Please enter a password: "
  password <- Password . T.strip <$> T.getLine
  print $ makeUser username password

type Test e s a = ExceptT e (State s) a

runTests :: Test Error Int () -> (Either Error (), Int)
runTests = flip runState 1 . runExceptT

assertEq :: (Eq a, Show a) => a -> a -> Test Error Int ()
assertEq actual expected = do
  n <- get
  if actual /= expected
    then throwError $ Error ["Test " <> showT n <> ", Expected: " <> showT expected <> ", But got: " <> showT actual]
    else do
      put (n + 1)
      return ()

printTestResult :: (Either Error (), Int) -> IO ()
printTestResult (Left err, _) = T.putStrLn $ showT err
printTestResult (Right _, n) = T.putStrLn ("All " <> showT (n - 1) <> " tests passed.")

test :: IO ()
test = printTestResult . runTests $ do
  assertEq (checkLength' "") $ Failure passwordLengthErr
  assertEq (checkLength' $ T.replicate 9 "1") $ Failure passwordLengthErr
  let s10 = T.replicate 10 "1"
  assertEq (checkLength' s10) $ Success s10
  assertEq (checkLength' (s10 <> "1")) $ Success (s10 <> "1")
  let s20 = s10 <> s10
  assertEq (checkLength' s20) $ Success s20
  assertEq (checkLength' (s20 <> "1")) $ Failure passwordLengthErr
  assertEq (requireAlphaNum "") $ Success ""
  assertEq (requireAlphaNum "Abc") $ Success "Abc"
  assertEq (requireAlphaNum "''") $ Failure alphaNumErr
  assertEq (requireAlphaNum "a_b-c") $ Failure alphaNumErr
  assertEq (validatePassword $ Password "asdfasdfasdf") . Success $ Password "asdfasdfasdf"
  assertEq (validatePassword $ Password "asdfasdf") $ Failure passwordLengthErr
  assertEq (validatePassword $ Password ",,,,,,,,,,,") $ Failure alphaNumErr
  assertEq (validateUsername $ Username "asdfasdf") . Success $ Username "asdfasdf"
  assertEq (validateUsername $ Username "as") $ Failure usernameLengthErr
  assertEq (validateUsername $ Username ",,,,,,,,,,,") $ Failure alphaNumErr
  where
    checkLength' = checkLength 10 20
    passwordLengthErr = Error ["Input must be at least 10 and at most 20 characters long."]
    usernameLengthErr = Error ["Input must be at least 3 and at most 15 characters long."]
    alphaNumErr = Error ["Cannot contain whitespace or special characters."]

-- exercise 24
-- You can write the Either version of validatePassword in applicative style
-- because all Either is a Monad and all Monads are Applicatives.

-- exercise 25
applicativeIO :: IO ()
applicativeIO = do
  print =<< checkAnagram <$> promptWord1 <*> promptWord2
  where
    checkAnagram xs ys = sort xs == sort ys
    promptWord1 = putStr "Please enter a word: " *> getLine
    promptWord2 = putStr "Please enter a second word: " *> getLine

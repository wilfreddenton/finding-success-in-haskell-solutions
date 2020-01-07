{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.Coerce
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Validation
import Prelude hiding (error)

newtype Username = Username Text deriving (Eq, Show)

newtype Password = Password Text deriving (Eq, Show)

newtype Error = Error (NonEmpty Text) deriving (Eq, Show, Semigroup)

type Rule a = a -> Validation Error a

error :: Text -> Error
error err = Error (err :| [])

showT :: (Show a) => a -> Text
showT = T.pack . show

checkLength :: Int -> Int -> Rule Text
checkLength lo hi t =
  if (&&) <$> (lo <=) <*> (hi >=) $ T.length t
    then Success t
    else Failure $ error ("Input must be at least " <> showT lo <> " and at most " <> showT hi <> " characters long.")

requireAlphaNum :: Rule Text
requireAlphaNum t =
  if T.all isAlphaNum t
    then Success t
    else Failure $ error "Cannot contain whitespace or special characters."

validateUsername :: Rule Username
validateUsername username =
  (coerce requireAlphaNum :: Rule Username) username
    *> (coerce $ checkLength 3 15 :: Rule Username) username

validatePassword :: Rule Password
validatePassword password =
  (coerce requireAlphaNum :: Rule Password) password
    *> (coerce $ checkLength 10 20 :: Rule Password) password

usernameErrors :: Rule Username
usernameErrors username = _Failure %~ (error "Invalid username:" <>) $ validateUsername username

passwordErrors :: Rule Password
passwordErrors password = _Failure %~ (error "Invalid password:" <>) $ validatePassword password

data User = User Username Password deriving (Show)

makeUser :: Validate v => Username -> Password -> v Error User
makeUser username password = _Validation # (User <$> usernameErrors username <*> passwordErrors password)

display :: Username -> Password -> IO ()
display username password = case makeUser username password of
  Left err -> T.putStr . T.unlines . NE.toList $ coerce err
  Right (User username' _) -> T.putStrLn ("Welcome, " <> coerce @Username @Text username')

main :: IO ()
main = do
  putStr "Please enter a username: "
  username <- Username . T.strip <$> T.getLine
  putStr "Please enter a password: "
  password <- Password . T.strip <$> T.getLine
  display username password

type Test e s a = ExceptT e (State s) a

runTests :: Test Error Int () -> (Either Error (), Int)
runTests = flip runState 1 . runExceptT

assertEq :: (Eq a, Show a) => a -> a -> Test Error Int ()
assertEq actual expected = do
  n <- get
  if actual /= expected
    then throwError $ error ("Test " <> showT n <> ", Expected: " <> showT expected <> ", But got: " <> showT actual)
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
    passwordLengthErr = error "Input must be at least 10 and at most 20 characters long."
    usernameLengthErr = error "Input must be at least 3 and at most 15 characters long."
    alphaNumErr = error "Cannot contain whitespace or special characters."

class LiftAB f where

  liftA :: a -> f a b

  liftB :: b -> f a b

instance LiftAB Either where

  liftA = Left

  liftB = Right

instance LiftAB Validation where

  liftA = Failure

  liftB = Success

class MaybeAB f where

  maybeA :: f a b -> Maybe a

  maybeB :: f a b -> Maybe b

instance MaybeAB Either where

  maybeA (Left a) = Just a
  maybeA (Right _) = Nothing

  maybeB (Left _) = Nothing
  maybeB (Right b) = Just b

instance MaybeAB Validation where

  maybeA (Failure a) = Just a
  maybeA (Success _) = Nothing

  maybeB (Failure _) = Nothing
  maybeB (Success b) = Just b

-- exercise 31
data These a b = This a | That b | These a b

instance LiftAB These where

  liftA = This

  liftB = That

instance MaybeAB These where

  maybeA (This a) = Just a
  maybeA (That _) = Nothing
  maybeA (These a _) = Just a

  maybeB (This _) = Nothing
  maybeB (That b) = Just b
  maybeB (These _ b) = Just b

-- exercise 32
validatePasswordLength :: Validate v => Password -> v Error Password
validatePasswordLength password = _Validation # (coerce $ checkLength 10 20 :: Rule Password) password

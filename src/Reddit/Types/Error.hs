{-# LANGUAGE OverloadedStrings #-}
module Reddit.Types.Error
  ( RedditError (..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Data.Vector ((!?))
import Network.API.Builder.Receive
import Prelude
import qualified Data.Vector as V

data RedditError
  = RedditError Object
  | FailError Text
  | InvalidResponseError
  | CaptchaError Text
  | CredentialsError
  | RateLimitError Integer Text
  | NoSubredditSpecified
  | NoURLSpecified
  | NoName
  | NoText Text
  | AlreadySubmitted
  | CommentDeleted
  | LinkDeleted
  | BadSubredditName
  | TooManyRequests
  deriving (Show, Eq)

instance FromJSON RedditError where
  parseJSON = withObject "RedditError" $ \o -> do
    Array errors <- o .: "json" >>= (.: "errors")
    case errors !? 0 of
      Just (Array e) -> case V.toList e of
        (String "WRONG_PASSWORD" : _) -> do
          pure CredentialsError
        (String "USER_REQUIRED" : _) -> do
          pure CredentialsError
        (String "RATELIMIT" : String d : _) -> do
          r <- (o .: "json") >>= (.: "ratelimit")
          pure (RateLimitError r d)
        (String "SUBREDDIT_REQUIRED" : _) -> do
          pure NoSubredditSpecified
        (String "ALREADY_SUB" : _) -> do
          pure AlreadySubmitted
        (String "NO_URL" : _) -> do
          pure NoURLSpecified
        (String "NO_NAME" : _) -> do
          pure NoName
        (String "NO_TEXT" : _ : String f : _) -> do
          pure (NoText f)
        (String "COMMENT_DELETED" : _) -> do
          pure CommentDeleted
        (String "DELETED_LINK" : _) -> do
          pure LinkDeleted
        (String "BAD_SR_NAME" : _) -> do
          pure BadSubredditName
        (String "BAD_CAPTCHA" : _) -> do
          CaptchaError <$> (o .: "json" >>= (.: "captcha"))
        _ -> pure $ RedditError o
      _ -> mempty

instance ErrorReceivable RedditError where
  receiveError = useErrorFromJSON

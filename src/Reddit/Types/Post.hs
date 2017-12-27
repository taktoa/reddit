{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Reddit.Types.Post
  ( module Reddit.Types.Post -- FIXME: specific export list
  ) where

import           Reddit.Parser
import           Reddit.Types.Listing
import           Reddit.Types.Reddit
import           Reddit.Types.Subreddit
import           Reddit.Types.Thing
import           Reddit.Types.User
import           Reddit.Utilities

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.Text                 (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Network.API.Builder.Query
import           Prelude

newtype PostID
  = PostID Text
  deriving (Eq, Ord, Show, Read)

instance FromJSON PostID where
  parseJSON = withText "PostID" $ \s -> do
    PostID <$> stripPrefix postPrefix s

instance FromJSON (POSTWrapped PostID) where
  parseJSON = withObject "POSTWrapped PostID" $ \o -> do
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "id"))

data Post
  = Post
    { postID           :: PostID
    , postTitle        :: Text
    , postPermalink    :: Text
    , postAuthor       :: Username
    , postScore        :: Integer
    , postCreated      :: UTCTime
    , postContent      :: PostContent
    , postCommentCount :: Integer
    , postLiked        :: Maybe Bool
    , postFlairText    :: Maybe Text
    , postFlairClass   :: Maybe Text
    , postDomain       :: Text
    , postGilded       :: Integer
    , postNSFW         :: Bool
    , postSubreddit    :: SubredditName
    , postSubredditID  :: SubredditID
    }
  deriving (Eq, Show, Read)

instance FromJSON Post where
  parseJSON = withObject "Post" $ \o -> do
    o `ensureKind` postPrefix
    d <- o .: "data"

    postID           <- d .: "id"
    postTitle        <- d .: "title"
    postPermalink    <- d .: "permalink"
    postAuthor       <- d .: "author"
    postScore        <- d .: "score"
    postCreated      <- posixSecondsToUTCTime . fromInteger
                        <$> d .: "created_utc"
    postContent      <- buildContent
                        <$> d .: "is_self"
                        <*> d .:? "selftext"
                        <*> d .:? "selftext_html"
                        <*> d .: "url"
    postCommentCount <- d .: "num_comments"
    postLiked        <- d .:? "likes"
    postFlairText    <- d .:? "link_flair_text"
    postFlairClass   <- d .:? "link_flair_css_class"
    postDomain       <- d .: "domain"
    postGilded       <- d .: "gilded"
    postNSFW         <- d .: "over_18"
    postSubreddit    <- d .: "subreddit"
    postSubredditID  <- d .: "subreddit_id"

    pure (Post {..})

data PostContent
  = SelfPost Text Text
  | Link Text
  | TitleOnly
  deriving (Eq, Show, Read)

buildContent :: Bool -> Maybe Text -> Maybe Text -> Maybe Text -> PostContent
buildContent False _         _           (Just url) = Link url
buildContent True  (Just s)  (Just html) _          = SelfPost (unescape s) html
buildContent True  (Just "") Nothing     _          = TitleOnly
buildContent _     _         _           _          = undefined -- FIXME

instance Thing Post where
  fullName p = let (PostID pID) = postID p
               in mconcat [postPrefix , "_", pID]

instance Thing PostID where
  fullName (PostID pID) = mconcat [postPrefix , "_", pID]

instance ToQuery PostID where
  toQuery k v = [(k, fullName v)]

type PostListing = Listing PostID Post

postPrefix :: Text
postPrefix = "t3"

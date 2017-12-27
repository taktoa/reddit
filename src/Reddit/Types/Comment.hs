{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Reddit.Types.Comment where

import           Reddit.Parser
import           Reddit.Types.Listing
import           Reddit.Types.Post
import           Reddit.Types.Reddit
import           Reddit.Types.Subreddit
import           Reddit.Types.Thing
import           Reddit.Types.User
import           Reddit.Utilities

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types          (Parser)
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Traversable
import qualified Data.Vector               as Vector
import           Network.API.Builder.Query
import           Prelude

newtype CommentID = CommentID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON CommentID where
  parseJSON = withText "CommentID" $ \s -> do
    CommentID <$> stripPrefix commentPrefix s

instance ToJSON CommentID where
  toJSON (CommentID s) = toJSON (commentPrefix <> s)

instance Thing CommentID where
  fullName (CommentID cID) = Text.concat [commentPrefix, "_", cID]

instance ToQuery CommentID where
  toQuery k v = [(k, fullName v)]

instance FromJSON (POSTWrapped CommentID) where
  parseJSON = withObject "POSTWrapped CommentID" $ \o -> do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case Vector.toList ts of
      [v] -> POSTWrapped <$> ((v .: "data") >>= (.: "id"))
      _   -> mempty

data CommentReference
  = Reference Integer [CommentID]
  | Actual Comment
  deriving (Show, Read, Eq)

instance FromJSON CommentReference where
  parseJSON = withObject "CommentReference" $ \o -> do
    k <- o .: "kind"
    case k of
      String "t1"   -> Actual <$> parseJSON (Object o)
      String "more" -> Reference
                       <$> ((o .: "data") >>= (.: "count"))
                       <*> ((o .: "data") >>= (.: "children"))
      _             -> mempty

instance FromJSON (POSTWrapped [CommentReference]) where
  parseJSON = withObject "PostWrapped [CommentReference]" $ \o -> do
    cs <- (o .: "json") >>= (.: "data") >>= (.: "things")
    POSTWrapped <$> parseJSON cs

-- | @isReference c@ is 'True' if @c@ is an actual comment, 'False' otherwise.
isActual :: CommentReference -> Bool
isActual (Actual _) = True
isActual _          = False

-- | @isReference c@ is 'True' if @c@ is a reference, 'False' otherwise.
isReference :: CommentReference -> Bool
isReference (Reference _ _) = True
isReference _               = False

data Comment
  = Comment
    { commentID           :: CommentID
    , score               :: Maybe Integer
    , subredditID         :: SubredditID
    , subreddit           :: SubredditName
    , gilded              :: Integer
    , saved               :: Bool
    , author              :: Username
    , authorFlairCSSClass :: Maybe Text
    , authorFlairText     :: Maybe Text
    , body                :: Text
    , bodyHTML            :: Text
    , replies             :: Listing CommentID CommentReference
    , created             :: UTCTime
    , edited              :: Maybe UTCTime
    , parentLink          :: PostID
    , inReplyTo           :: Maybe CommentID
    }
  deriving (Show, Read, Eq)

instance Thing Comment where
  fullName c = fullName (commentID c)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    o `ensureKind` commentPrefix
    d <- o .: "data"
    Comment <$> d .: "id"
            <*> d .:? "score"
            <*> d .: "subreddit_id"
            <*> d .: "subreddit"
            <*> d .: "gilded"
            <*> d .: "saved"
            <*> d .: "author"
            <*> d .:? "author_flair_css_class"
            <*> d .:? "author_flair_text"
            <*> (unescape <$> d .: "body")
            <*> d .: "body_html"
            <*> d .: "replies"
            <*> (posixSecondsToUTCTime . fromInteger <$> d .: "created_utc")
            <*> ((fmap (posixSecondsToUTCTime . fromInteger) <$> d .: "edited")
                 <|> pure Nothing)
            <*> (d .: "link_id" >>= parseJSON)
            <*> (d .:? "parent_id"
                 >>= \v -> traverse parseJSON v <|> pure Nothing)

instance FromJSON (POSTWrapped Comment) where
  parseJSON = withObject "POSTWrapped Comment" $ \o -> do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case Vector.toList ts of
      [c] -> POSTWrapped <$> parseJSON c
      _   -> mempty

treeSubComments :: CommentReference -> [CommentReference]
treeSubComments = go
  where
    go a@(Actual c)     = a : concatMap treeSubComments (getCS (replies c))
    go (Reference _ rs) = map (\r -> Reference 1 [r]) rs

    getCS (Listing _ _ cs) = cs

isDeleted :: Comment -> Bool
isDeleted = (== Username "[deleted]") . author

data PostComments = PostComments Post [CommentReference]
  deriving (Show, Read, Eq)

instance FromJSON PostComments where
  parseJSON = withArray "PostComments" $ \a -> do
    case Vector.toList a of
      (postListing : commentListing : _) -> do
        Listing _ _ [post]   <- parseJSON postListing
                                :: Parser (Listing PostID Post)
        Listing _ _ comments <- parseJSON commentListing
                                :: Parser (Listing CommentID CommentReference)
        return $ PostComments post comments
      _ -> mempty

type CommentListing = Listing CommentID Comment

commentPrefix :: Text
commentPrefix = "t1"

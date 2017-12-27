-- | Contains user-related actions, like finding friends or retrieving a
--   user's comments or information.
module Reddit.Actions.User
  ( getUserInfo
  , aboutMe
  , getUserComments
  , getUserComments'
  , getUserPosts
  , getUserPosts'
  , isUsernameAvailable
  , getBlockedUsers
  , getFriends
  , lookupUserFlair
  , setUserFlair ) where

import qualified Reddit.Routes.User        as Route
import           Reddit.Types.Comment
import           Reddit.Types.Empty
import           Reddit.Types.Error
import           Reddit.Types.Flair        hiding (user)
import           Reddit.Types.Listing
import           Reddit.Types.Options
import           Reddit.Types.Post
import           Reddit.Types.Reddit
import           Reddit.Types.Subreddit
import           Reddit.Types.User

import           Control.Monad
import           Data.Default.Class
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Network.API.Builder.Error

-- | Get the information Reddit exposes on user behind the specified username
getUserInfo :: (Monad m)
            => Username
            -> RedditT m User
getUserInfo = runRoute . Route.aboutUser

-- | Get the listing of comments authored by the specified user.
getUserComments :: (Monad m)
                => Username
                -> RedditT m CommentListing
getUserComments = getUserComments' def

-- | Get the listing of comments authored by the specified user, with Options.
getUserComments' :: (Monad m)
                 => Options CommentID
                 -> Username
                 -> RedditT m CommentListing
getUserComments' opts user = runRoute $ Route.userComments opts user

-- | Get the listing of posts authored by the specified user.
getUserPosts :: (Monad m)
             => Username
             -> RedditT m PostListing
getUserPosts = getUserPosts' def

-- | Get the listing of posts authored by the specified user, with Options.
getUserPosts' :: (Monad m)
              => Options PostID
              -> Username
              -> RedditT m PostListing
getUserPosts' opts user = runRoute $ Route.userPosts opts user

-- | Check whether the specified username is still available or has been taken.
isUsernameAvailable :: (Monad m)
                    => Username
                    -> RedditT m Bool
isUsernameAvailable = runRoute . Route.usernameAvailable

-- | Get information of the currently-logged-in user.
aboutMe :: (Monad m)
        => RedditT m User
aboutMe = runRoute Route.aboutMe

-- | Get users blocked by the currently-logged-in user.
getBlockedUsers :: (Monad m)
                => RedditT m [Relationship]
getBlockedUsers = do
  UserList rs <- runRoute Route.blocked
  pure rs

-- | Get friends of the currently-logged-in user.
getFriends :: (Monad m)
           => RedditT m [Relationship]
getFriends = do
  UserList rs <- runRoute Route.friends
  pure rs

-- | Check if a user has chosen (or been assign) user flair on a particular
--   subreddit. Requires moderator privileges on the specified subreddit.
lookupUserFlair :: (Monad m)
                => SubredditName
                -> Username
                -> RedditT m Flair
lookupUserFlair r u = do
  res <- flistToListing <$> runRoute (Route.lookupUserFlair r u)
  case res of
    Listing _ _ [f] -> pure f
    _               -> failWith (APIError InvalidResponseError)

-- | Set a user's flair on the specified subreddit. Requires moderator
--   privileges on the specified subreddit.
setUserFlair :: (Monad m)
             => SubredditName
             -> Username
             -> Text
             -> Text
             -> RedditT m ()
setUserFlair r u txt cls
  = if Text.length txt > 64
      then fail "Flair text too long!"
      else nothing $ runRoute $ Route.setUserFlair r u txt cls

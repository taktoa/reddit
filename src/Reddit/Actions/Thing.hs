-- | Contains actions which are applicable to both Posts and Comments,
--   like replying, deleting and reporting.
module Reddit.Actions.Thing
  ( Reddit.Actions.Thing.reply
  , Reddit.Actions.Thing.delete
  , Reddit.Actions.Thing.report
  ) where

import Reddit.Types
import Reddit.Types.Empty
import Reddit.Types.Reddit
import qualified Reddit.Routes.Thing as Route

import Data.Text (Text)

-- | Reply to a something (a post \/ comment \/ message)
reply :: (Monad m, Thing thing)
      => thing
      -- ^ Thing to reply to
      -> Text
      -- ^ Response contents
      -> RedditT m CommentID
reply t b = do
  POSTWrapped res <- runRoute $ Route.reply t b
  return res

-- | Delete something you created. Note that this is different to removing
--   a post \/ comment as a moderator action. Deleting something you don't
--   own won't error (but naturally won't delete anything either).
delete :: (Monad m, Thing thing)
       => thing
       -- ^ Thing to delete
       -> RedditT m ()
delete = nothing . runRoute . Route.delete

-- | Report something.
report :: (Monad m, Thing thing)
       => thing
       -- ^ Thing to report
       -> Text
       -- ^ Reason for reporting
       -> RedditT m ()
report t r = nothing (runRoute (Route.report t r))

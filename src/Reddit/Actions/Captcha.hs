-- | Contains Captcha-related actions. Reddit sometimes requests CAPTCHAs in
--   order to prevent spambots, and you can use this module to get more info
--   on them. Unfortunately the library doesn't yet support answering CAPTCHAs
--   on a widespread scale, and you have to use slightly modified variants of
--   other functions to convince Reddit that you aren't a robot.
module Reddit.Actions.Captcha
  ( needsCaptcha
  , newCaptcha ) where

import Reddit.Types.Captcha
import Reddit.Types.Reddit
import qualified Reddit.Routes.Captcha as Route

-- | Find out if the account currently logged in requires a captcha to be
--   submitted for certain requests, like sending a private message or
--   submitting a post.
needsCaptcha :: Monad m => RedditT m Bool
needsCaptcha = runRoute Route.needsCaptcha

-- | Returns the ID of a CAPTCHA to be completed.
--   The CAPTCHA image for which can be found at <http://reddit.com/captcha/ID>.
newCaptcha :: Monad m => RedditT m CaptchaID
newCaptcha = do
  POSTWrapped c <- runRoute Route.newCaptcha
  return c

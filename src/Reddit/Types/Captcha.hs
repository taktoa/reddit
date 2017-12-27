{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Reddit.Types.Captcha where

import           Reddit.Types.Reddit

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.Text           (Text)
import           Network.API.Builder
import           Prelude

newtype CaptchaID = CaptchaID Text
  deriving (Read, Show, Eq, Ord)

instance FromJSON CaptchaID where
  parseJSON j = CaptchaID <$> parseJSON j

instance ToJSON CaptchaID where
  toJSON (CaptchaID t) = toJSON t

instance FromJSON (POSTWrapped CaptchaID) where
  parseJSON (Object o) =
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "iden"))
  parseJSON _ = mempty

instance ToJSON (POSTWrapped CaptchaID) where
  toJSON (POSTWrapped (CaptchaID t))
    = let wrap key value = object [key .= value]
      in wrap "json" (wrap "data" (wrap "iden" (toJSON t)))

withCaptcha :: Route -> (CaptchaID, Text) -> Route
withCaptcha (Route pieces params meth) (CaptchaID i, c) =
  Route pieces (iden : captcha : params) meth
  where iden = ("iden" :: Text) =. i
        captcha = ("captcha" :: Text) =. c

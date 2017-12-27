{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Data.Default.Class
import           Reddit

runAnon :: Reddit a -> IO (Either (APIError RedditError) a)
runAnon =
  runRedditWith def { customUserAgent = Just "reddit haskell test suite" }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Reddit.Actions.FlairSpec where

import Reddit.Actions.Flair

import ConfigLoad
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Flair" $ do
  (reddit, _, subreddit) <- runIO loadConfig

  it "should be able to get the flair list" $ do
    res <- run reddit $ getFlairList subreddit
    res `shouldSatisfy` isRight


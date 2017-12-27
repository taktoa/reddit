{-# LANGUAGE OverloadedStrings #-}

module Reddit.Actions.FlairSpec where

import           Reddit
import           Utils

import           Test.Hspec

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Flair" $ do

  it "shouldn't be able to get the flair list anonymously" $ do
    res <- runAnon $ getFlairList $ R "gaming"
    res `shouldSatisfy` isLeft

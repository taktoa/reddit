{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Reddit.Actions.PostSpec where

import           Reddit.Actions.Post
import           Reddit.Types.Listing
import           Reddit.Types.Post
import           Reddit.Types.Subreddit (SubredditID (..), SubredditName (..))
import           Reddit.Types.User

import           ConfigLoad
import           Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Post" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to get info for a post" $ do
    res <- run reddit $ getPostInfo (PostID "z1c9z")
    res `shouldSatisfy` isRight
    case res of
      Left  _    -> expectationFailure "something failed"
      Right post -> do
        postAuthor post
          `shouldBe` Username "PresidentObama"
        postTitle post
          `shouldBe` "I am Barack Obama, President of the United States -- AMA"
        postSubredditID post
          `shouldBe` SubredditID "2qzb6"
        postSubreddit post
          `shouldBe` R "IAMA"
        postNSFW post
          `shouldBe` False

  it "should be able to get info for multiple posts" $ do
    res <- run reddit $ getPostsInfo [PostID "z1c9z", PostID "t0ynr"]
    res `shouldSatisfy` isRight

  it "should cope with getting info for no posts" $ do
    res <- run reddit $ getPostsInfo []
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) ->
        ps `shouldBe` []

  it "shouldn't be able to get a list of posts from invalid post IDs" $ do
    res <- run reddit $ getPostsInfo [PostID "z1c9z", PostID "nonsense"]
    res `shouldSatisfy` isLeft

  it "should be able to get mass PostIDs" $ do
    let a = replicate 100 $ PostID "z1c9z"
    res <- run reddit $ getPostsInfo a
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) ->
        length ps `shouldBe` length a

  it "should fail if it tries to get TOO many PostIDs" $ do
    let a = replicate 101 $ PostID "z1c9z"
    res <- run reddit $ getPostsInfo a
    res `shouldSatisfy` isLeft

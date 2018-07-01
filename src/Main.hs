{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Lens        (key, _String)
import qualified Data.Aeson.Types       as DAT
import qualified Data.ByteString        as B
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Vector as V
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics
import           GHC.Word
import qualified Network.Wreq           as W
import           System.Environment     (getEnv)

tokenUrl = "https://www.reddit.com/api/v1/access_token"
subreddit = ""

data Thread = Thread {
   names :: [String]
   } deriving (Generic, Show)
instance FromJSON Thread where
  parseJSON = withObject "data" $ \d -> do
    ddata      <- d     .: "data" :: DAT.Parser Object
    children   <- ddata .: "children" :: DAT.Parser [Object]
    threads    <- mapM (\i -> i .: "data") children
    names <- mapM (\i -> i .: "id") threads
    return Thread{ names=names }

-- jq 'fromjson | .[0] |.data | .children | .[0] | .data | .author'

data Comment = Comment {
  cnames :: [String]
  } deriving (Show)

data Comments = Comments [Comment]
                deriving (Show)

instance FromJSON Comment where
  parseJSON = withObject "data" $ \d -> do
    info <- d .: "data" :: DAT.Parser Object
    children <- info .: "children" :: DAT.Parser [Object]
    childrenData <- mapM (\x -> x .: "data" :: DAT.Parser Object) children
    authors <- mapM (\x -> x .: "author" :: DAT.Parser String) childrenData
    return Comment { cnames=authors }

instance FromJSON Comments where
  parseJSON = withArray "thread" $ \arr -> do
    comments <- mapM parseJSON (V.toList arr) :: DAT.Parser [Comment]

    return $ Comments comments

getHot :: W.Options -> String -> IO (W.Response LBS.ByteString)
getHot opts subreddit = W.getWith opts ("https://oauth.reddit.com/r/" ++ subreddit ++ "/hot.json")

getComments :: W.Options -> String -> String -> IO (W.Response LBS.ByteString)
getComments opts subreddit threadId =
  W.getWith opts ("https://oauth.reddit.com/r/" ++ subreddit ++ "/comments/" ++ threadId ++ ".json")

main :: IO ()
main = do
  -- env
  user         <- getEnv "REDDIT_USER_NAME"
  password     <- getEnv "REDDIT_USER_PASSWORD"
  clientSecret <- getEnv "REDDIT_CLIENT_SECRET"
  cliendId     <- getEnv "REDDIT_CLIENT_ID"

  -- token
  let opts = W.defaults & W.auth ?~ W.basicAuth (pack cliendId) (pack clientSecret)
  r <- W.postWith opts tokenUrl [ ("grant_type" :: B.ByteString) W.:= ("password"      :: B.ByteString)
                                , ("username"   :: B.ByteString) W.:= ("quakquakquak"  :: B.ByteString)
                                , ("password"   :: B.ByteString) W.:= ((pack password) :: B.ByteString) ]
  let token = r ^. W.responseBody . key "access_token" . _String
      authOpts = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 token)

  -- get threads on page
  res <- getHot authOpts subreddit

  let permalinks = eitherDecode (res ^. W.responseBody) :: Either String Thread

  let comments = case permalinks of
        Right (Thread (n:names)) -> do
          res <- getComments authOpts subreddit n
          return $ Data.ByteString.Char8.concat . LBS.toChunks $ res ^. W.responseBody
        Left a                   -> return (pack a)
      test = case permalinks of
        Right (Thread (n:names)) -> do
          getComments authOpts subreddit n

  res2 <- test
  let decoded = eitherDecode (res2 ^. W.responseBody) :: Either String Comments

  case decoded of
    Right (things) -> print . show $ things
    Left err -> print err

  return ()

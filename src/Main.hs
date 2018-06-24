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
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics
import           GHC.Word
import qualified Network.Wreq           as W
import           System.Environment     (getEnv)

tokenUrl = "https://www.reddit.com/api/v1/access_token"

data Thread = Thread {
   permalinks :: [String]
   } deriving (Generic, Show)
instance FromJSON Thread where
  parseJSON = withObject "data" $ \d -> do
    ddata      <- d     .: "data" :: DAT.Parser Object
    children   <- ddata .: "children" :: DAT.Parser [Object]
    threads    <- mapM (\i -> i .: "data") children
    permalinks <- mapM (\i -> i .: "permalink") threads
    return Thread{ permalinks=permalinks }

getHot :: W.Options -> String -> IO (W.Response LBS.ByteString)
getHot opts subreddit = W.getWith opts ("https://oauth.reddit.com/r/" ++ subreddit ++ "/hot.json")

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

  -- do
  res <- getHot authOpts "the_donald"

  let permalinks = eitherDecode (res ^. W.responseBody) :: Either String Thread
  print . show $ permalinks

  return ()

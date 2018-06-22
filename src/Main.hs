{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           GHC.Generics
import           System.Environment     (getEnv)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson.Types       as DAT
import           Data.Aeson.Lens        (key, _String)
import qualified Data.ByteString        as B
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Word
import qualified Network.Wreq           as W

tokenUrl = "https://www.reddit.com/api/v1/access_token"

data Thread = Thread {
  permalink :: String
  } deriving (Generic)
instance FromJSON Thread


children :: Value -> DAT.Parser [Thread]
children = withObject "children" $ \o -> o .: "children"

datum :: Value -> DAT.Parser children
datum = withObject "data" $ \x -> x .: "data"

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
                              , ("username"   :: B.ByteString)   W.:= ("quakquakquak"  :: B.ByteString)
                              , ("password"   :: B.ByteString)   W.:= ((pack password) :: B.ByteString) ]
  let token = r ^. W.responseBody . key "access_token" . _String
      authOpts = W.defaults & W.auth ?~ W.oauth2Bearer (encodeUtf8 token)

  -- do
  res <- getHot authOpts "the_donald"
  print $ res ^. W.responseBody

  return ()

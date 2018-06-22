{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment     (getEnv)
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson             (decode)
import           Data.Aeson.Lens        (key, _String)
import qualified Data.ByteString        as B
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy   as LBS
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Word
import           Network.Wreq

tokenUrl = "https://www.reddit.com/api/v1/access_token"

getHot :: Options -> String -> IO (Response LBS.ByteString)
getHot opts subreddit = getWith opts ("https://oauth.reddit.com/r/" ++ subreddit ++ "/hot")

main :: IO ()
main = do
  -- env
  user         <- getEnv "REDDIT_USER_NAME"
  password     <- getEnv "REDDIT_USER_PASSWORD"
  clientSecret <- getEnv "REDDIT_CLIENT_SECRET"
  cliendId     <- getEnv "REDDIT_CLIENT_ID"

  print user
  -- token
  let opts = defaults & auth ?~ basicAuth (pack cliendId) (pack clientSecret)
  r <- postWith opts tokenUrl [ ("grant_type" :: B.ByteString) := ("password"      :: B.ByteString)
                              , ("username"   :: B.ByteString) := ("quakquakquak"  :: B.ByteString)
                              , ("password"   :: B.ByteString) := ((pack password) :: B.ByteString) ]
  let token = r ^. responseBody . key "access_token" . _String
      authOpts = defaults & auth ?~ oauth2Bearer (encodeUtf8 token)

  -- do
  res <- getHot authOpts "the_donald"
  print $ res ^. responseBody

  return ()

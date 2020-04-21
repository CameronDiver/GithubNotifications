module GithubNotifications.API where

import           Control.Exception             as E
import           Control.Monad
import           Control.Lens
import           Data.ByteString
import           Data.ByteString.UTF8          as BU
import qualified Data.ByteString.Lazy          as BL
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.Encoding
import           Network.HTTP.Client
import           Network.Wreq                  as W
import           Text.Printf
import           System.Environment
import           System.Exit

import           GithubNotifications.Auth
import           GithubNotifications.Notification

notificationEndpoint = "https://api.github.com/notifications"

type NotificationResponse = Response [Notification]

getNotifications :: GithubAuthentication -> IO [Notification]
getNotifications ghAuth = do
  let (GithubAuthentication user pass) = ghAuth
  let opts = defaults & auth ?~ basicAuth (encodeUtf8 user) (encodeUtf8 pass)
  req <- asJSON =<< getWith opts notificationEndpoint
  return (req ^. W.responseBody)

getUrlFromNotification :: GithubAuthentication -> Notification -> IO ByteString
getUrlFromNotification ghAuth n
  | nType sub == "PullRequest" = do
    pr <- getPullRequestUrl ghAuth $ url (sub :: NotificationSubject)
    return $ encodeUtf8 $ html_url pr
  | otherwise = return $ encodeUtf8 $ url (sub :: NotificationSubject)
  where sub = subject n

getPullRequestUrl :: GithubAuthentication -> Text -> IO PullRequest
getPullRequestUrl ghAuth url = do
  let (GithubAuthentication user pass) = ghAuth
  let opts = defaults & auth ?~ basicAuth (encodeUtf8 user) (encodeUtf8 pass)
  req <- asJSON =<< getWith opts (T.unpack url)
  return (req ^. W.responseBody)

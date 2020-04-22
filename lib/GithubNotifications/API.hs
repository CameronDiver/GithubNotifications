module GithubNotifications.API where

import           Control.Lens
import           Data.ByteString
import           Data.Text                     as T
import           Data.Text.Encoding
import           Network.HTTP.Client
import           Network.Wreq                  as W

import           GithubNotifications.Auth
import           GithubNotifications.Notification

notificationEndpoint :: String
notificationEndpoint = "https://api.github.com/notifications"

type NotificationResponse = Response [Notification]

getNotifications :: GithubAuthentication -> IO [Notification]
getNotifications (GithubAuthentication u pass) = do
  let opts = defaults & auth ?~ basicAuth (encodeUtf8 u) (encodeUtf8 pass)
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
getPullRequestUrl ghAuth u = do
  let (GithubAuthentication usr pass) = ghAuth
  let opts = defaults & auth ?~ basicAuth (encodeUtf8 usr) (encodeUtf8 pass)
  req <- asJSON =<< getWith opts (T.unpack u)
  return (req ^. W.responseBody)

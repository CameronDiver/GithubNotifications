module GithubNotifications.API where

import           Control.Exception             as E
import           Control.Monad
import           Control.Lens
import           Data.Maybe
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

getNotifications :: IO [Notification]
getNotifications = do
  ghAuth <- getAuth
  let (GithubAuthentication user pass) =
        fromMaybe (error "Could not read authorisation from auth.json") ghAuth
  let opts = defaults & auth ?~ basicAuth (encodeUtf8 user) (encodeUtf8 pass)
  req <- asJSON =<< getWith opts notificationEndpoint
  return (req ^. W.responseBody)



module Main where

import           Control.Lens
import           Network.Wreq                  as W
import           Text.Printf

import           GithubNotifications.Notification

getNotifications :: IO [NotificationWithUrl]
getNotifications = do
  req <- asJSON =<< get "http://localhost:3000/notifications"
  return (req ^. W.responseBody)

main :: IO ()
main = do
  putStrLn "Github notifications: https://github.com/notifications"
  body <- getNotifications
  mapM_ showNotification body

showNotification :: NotificationWithUrl -> IO ()
showNotification (NotificationWithUrl u n) = printf "%s\n\t%s\n" (show n) u

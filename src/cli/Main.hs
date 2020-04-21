module Main where

import           Control.Lens
import           Data.Text
import           Network.Wreq                  as W

import           GithubNotifications.Notification

getNotifications :: IO [Notification]
getNotifications = do
  req <- asJSON =<< get "http://localhost:3000/notifications"
  return (req ^. W.responseBody)

main :: IO ()
main = do
  putStrLn "Github notifications:"
  body <- getNotifications
  mapM_ showNotification body

showNotification :: Notification -> IO ()
showNotification = print

module Main where

import           Control.Exception             as E
import           Data.Text

import           GithubNotifications.API
import           GithubNotifications.Auth
import           GithubNotifications.Notification

main :: IO ()
main = do
  initConfigDirectory
  putStrLn "Github notifications:"
  (do
      body <- getNotifications
      mapM_ showNotification body
    )
    `E.catch` handler

handler e@(SomeException f) = putStrLn ("An error occured:\n\t" ++ show f)

showNotification :: Notification -> IO ()
showNotification n = putStrLn $ show n ++ "\n  " ++ unpack ((url . subject) n)

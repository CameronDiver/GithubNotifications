module Main where

import           Control.Exception             as E
import           Data.Text
import           Data.ByteString.UTF8          as BU
import           Data.Maybe

import           GithubNotifications.API
import           GithubNotifications.Auth
import           GithubNotifications.Notification

main :: IO ()
main = do
  initConfigDirectory
  putStrLn "Github notifications:"
  (do
      ghAuth <-
        fromMaybe (error "Could not read authorisation from auth.json")
          <$> getAuth
      body <- getNotifications ghAuth
      mapM_ (showNotification ghAuth) body
    )
    `E.catch` handler

handler e@(SomeException f) = putStrLn ("An error occured:\n\t" ++ show f)

showNotification :: GithubAuthentication -> Notification -> IO ()
showNotification ghAuth n = do
  url <- getUrlFromNotification ghAuth n
  putStrLn $ show n ++ "\n  " ++ BU.toString url

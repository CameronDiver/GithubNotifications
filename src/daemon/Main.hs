module Main where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Aeson
import           Data.Maybe
import qualified Network.Wai                   as W
import           System.IO
import           Web.Firefly

import           GithubNotifications.Auth
import           GithubNotifications.API
import           GithubNotifications.Notification


-- 10 Minutes
fetchInterval = 60 * 10 ^ 7

type NotificationsVar = MVar [Notification]

fetchAndStore :: GithubAuthentication -> NotificationsVar -> IO ()
fetchAndStore ghAuth notifs = forever $ do
  putStrLn "Requesting notifications"
  getNotifications ghAuth >>= store
  threadDelay fetchInterval
 where
  store x = do
    empty <- isEmptyMVar notifs
    if empty then putMVar notifs x else void $ swapMVar notifs x

main = do
  initConfigDirectory
  maybeAuth <- getAuth
  when (isNothing maybeAuth)
    $ error "Could not read authorisation file auth.json"

  putStrLn "Running notification collection daemon..."
  var <- newEmptyMVar
  forkIO $ fetchAndStore (fromJust maybeAuth) var

  -- TODO: make the port configurable
  run 3000 $ app var



app :: NotificationsVar -> App ()
app notifs = route "/notifications" $ notificationsHandler notifs

notificationsHandler :: NotificationsVar -> Handler W.Response
notificationsHandler notifs = do
  lift $ putStrLn "Received notification request"
  notificationList <- lift $ tryReadMVar notifs
  return . toResponse . Json $ fromMaybe [] notificationList

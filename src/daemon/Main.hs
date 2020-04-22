module Main where

import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.ByteString                ( )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Maybe
import qualified Network.Wai                   as W
import           Web.Firefly

import           GithubNotifications.Auth
import           GithubNotifications.API
import           GithubNotifications.Notification


-- 10 Minutes
fetchInterval :: Int
fetchInterval = 60 * (10 ^ (7 :: Int))

type NotificationsVar = MVar [NotificationWithUrl]

fetchAndStore :: GithubAuthentication -> NotificationsVar -> IO ()
fetchAndStore ghAuth notifs = forever $ do
  putStrLn "Requesting notifications"

  ns   <- getNotifications ghAuth
  -- Fetch all the URLs concurrently
  urls <- mapConcurrently (getUrlFromNotification ghAuth) ns

  store $ zipWith (\a b -> NotificationWithUrl (decodeUtf8 a) b) urls ns

  --getNotifications ghAuth >>= store
  threadDelay fetchInterval
 where
  store x = do
    empty <- isEmptyMVar notifs
    if empty then putMVar notifs x else void $ swapMVar notifs x

main :: IO ()
main = do
  initConfigDirectory
  maybeAuth <- getAuth
  when (isNothing maybeAuth)
    $ error "Could not read authorisation file auth.json"

  putStrLn "Running notification collection daemon..."
  var <- newEmptyMVar
  _   <- forkIO $ fetchAndStore (fromJust maybeAuth) var

  -- TODO: make the port configurable
  run 7123 $ app var



app :: NotificationsVar -> App ()
app notifs = route "/notifications" $ notificationsHandler notifs

notificationsHandler :: NotificationsVar -> Handler W.Response
notificationsHandler notifs = do
  lift $ putStrLn "Received notification request"
  notificationList <- lift $ tryReadMVar notifs
  return . toResponse . Json $ fromMaybe [] notificationList

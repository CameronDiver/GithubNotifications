module GithubNotifications.Auth where
import           Control.Exception             as E
import           Control.Monad
import           Data.Aeson                    as A
import           Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.UTF8     as BU
import           Data.ByteString.Internal
import           Data.Text
import           Data.Maybe
import           GHC.Generics
import           System.Directory
import           System.Environment

import           Debug.Trace                    ( traceShow )

data GithubAuthentication = GithubAuthentication { username :: Text, token :: Text }
  deriving Generic
instance FromJSON GithubAuthentication


-- Maybe cache these lookups
getConfigDirectory :: IO BU.ByteString
getConfigDirectory = do
  -- Assume HOME exists, bigger problems if it doesn't...
  home     <- getEnv "HOME"
  maybeDir <- lookupEnv "XDG_CONFIG_HOME"
  return
    $  BU.fromString
    $  (fromMaybe (home ++ "/.config") maybeDir)
    ++ "/GithubNotifications/"

initConfigDirectory :: IO ()
initConfigDirectory = do
  confDir <- getConfigDirectory
  createDirectoryIfMissing True $ BU.toString confDir

authFile :: B.ByteString
authFile = "auth.json"

getAuth :: IO (Maybe GithubAuthentication)
getAuth = do
  confDir <- getConfigDirectory
  let path = B.append confDir authFile
  content <- B.readFile (BU.toString path)
  return $ A.decode content

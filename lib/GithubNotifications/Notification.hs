module GithubNotifications.Notification where

import           Data.Maybe
import           Data.Text                     as T
import           Data.ByteString.Lazy
import           Data.Aeson
import           GHC.Generics
import           Text.Printf

import           System.Console.Pretty          ( Color(..)
                                                , color
                                                , Style(..)
                                                , style
                                                )

-- TODO: Maybe change all of the underscore fields to camelCase

data Notification = Notification {
    id :: !Text
  , unread :: Bool
  , reason :: !Text
  , updated_at :: !Text
  , last_read_at :: Maybe Text
  , subject :: NotificationSubject
  , repository :: Repository
} deriving (Generic,FromJSON,ToJSON)

data NotificationSubject = NotificationSubject {
    title :: !Text
  , url :: !Text
  , commitUrl :: Maybe Text
  , nType :: !Text
} deriving (Show,Generic)

-- Write the instances by hand here, because of the "type" field
instance FromJSON NotificationSubject where
  parseJSON (Object v) =
    NotificationSubject
      <$> v
      .:  "title"
      <*> v
      .:  "url"
      <*> v
      .:? "latest_commit_url"
      <*> v
      .:  "type"

instance ToJSON NotificationSubject where
  toJSON (NotificationSubject t u c nT) =
    object ["title" .= t, "url" .= u, "latest_commit_url" .= c, "type" .= nT]


data Repository = Repository {
  -- id :: !Text (can't reuse)
  full_name :: !Text
} deriving (Show,Generic,FromJSON,ToJSON)


notificationReasonToEmoji :: Text -> Text
notificationReasonToEmoji t = T.append
  (case t of
    "review_requested" -> "👀"
    "security_alert"   -> "🛡️ "
    "assign"           -> "🛠"
    "author"           -> "✍️"
    "comment"          -> "💬"
  -- Questionable
    "invitation"       -> "📝"
    "subscribe"        -> "☑️"
    "mention"          -> "👋"
    "state_change"     -> "📩"
    "subscribed"       -> "📺"
    _                  -> t
  )
  " "

instance Show Notification where
  show (Notification id unread reason updated lra sub repo) = printf
    "%s%s: %s"
    (notificationReasonToEmoji reason)
    (color Green $ full_name repo)
    (style Bold $ title sub)

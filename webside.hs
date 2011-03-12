{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Happstack.Server (nullConf, simpleHTTP, ok, notFound, dir, FromReqURI(..), path, ServerPart, Response, toResponse)
import Happstack.Server.Monads (ServerPartT)

import           Text.Blaze ((!), toHtml)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

import Data.Time

import Control.Monad
import Control.Monad.Trans (liftIO)

import Maybe (listToMaybe)

day :: ZonedTime -> Day
day (ZonedTime (LocalTime d t) tz) = d

daysDiffThere :: ZonedTime -> ZonedTime -> Integer
daysDiffThere timeHere (ZonedTime (LocalTime d t) there) = 
  let now = zonedTimeToUTC timeHere
      ZonedTime (LocalTime dayThere timeThere) _ = utcToZonedTime there now
  in diffDays d dayThere

--from the BlazeHtml tutorial
appTemplate :: String  -> H.Html -> H.Html
appTemplate title body =
  H.html $ do
    H.head $ do
      H.title (H.string $ title ++ " - Reminded by Vegans")
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
    H.body $ do
      body

newtype Username = Username String
newtype UID = UID Int
              deriving (Show)
newtype RID = RID Int
              deriving (Show)
data Reminder = Reminder RID UID {-String-} String ZonedTime Bool 
              deriving (Show)
data ReminderInTime = ReminderInTime Reminder Integer

{- Query-related things -}


uidByEmail :: Connection -> String -> IO (Maybe UID)
uidByEmail c email = do
  r <- quickQuery' c "SELECT uid FROM usr WHERE email=?" [toSql (email :: String)]
  return $ listToMaybe $ map (UID . fromSql . head) r
  
getReminderInTime timeHere [rid, uid, body, when, sent] = 
  ReminderInTime 
    (Reminder (RID $ fromSql rid) (UID $ fromSql uid) (fromSql body) (fromSql when) (fromSql sent))
    (daysDiffThere timeHere (fromSql when))

upcomingRemindersByUid :: Connection -> UID -> IO [ReminderInTime]
upcomingRemindersByUid c (UID uidInt)  = do  
  timeHere <- getZonedTime
  r <- quickQuery' c "SELECT rid, uid, message, when_to_send, sent FROM reminder WHERE uid=? AND NOT sent" [toSql uidInt]
  return $ map (getReminderInTime timeHere) r



{- Serving -}

instance FromReqURI Username where
  fromReqURI name = Just $ Username name



userPage :: String -> (UID -> Connection -> ServerPartT IO Response) -> ServerPartT IO Response
userPage pageName fn = dir "user" $ path $ \name -> dir pageName $ do
  (c, possiblyUid) <- liftIO $ do
    c <- connectPostgreSQL "host=localhost dbname=rbv user=vegans password=local_login_password"

    possiblyUid <- uidByEmail c name
    
    return (c, possiblyUid)
  
  case possiblyUid of
    Nothing -> notFound $ toResponse $ appTemplate "No such user" $ H.p $ "That user doesn't exist."
    Just uid -> fn uid $ c

main :: IO ()
main =
  simpleHTTP nullConf $ msum
    [ mzero,
      userPage "reminders" remindersPage -- \(Username name) -> ok $ "Hi, " ++ name
      --dir "user" $ path $ \(Username name) -> ok $ "Hi" ++ name
      --dir "reminders_by_date" $ ok "Reminders by date",
      --dir "other" $ ok "Other" 
    ]


renderReminderRow :: [Integer] -> [ReminderInTime] -> H.Html
renderReminderRow range reminders = 
  H.tr $ forM_ range $ \dayDiff ->
    H.td $ forM_ (filter (\(ReminderInTime (Reminder _ _ _ _ sent) itsDayDiff) -> (not sent) && itsDayDiff == dayDiff) reminders) $
      \(ReminderInTime (Reminder _ _ body when sent) _) -> H.string $ body


remindersPage :: UID -> Connection -> ServerPartT IO Response
remindersPage uid c = do
  val <- liftIO $ do
    reminders <- upcomingRemindersByUid c uid
    return $ toResponse $ appTemplate "Reminders" $ H.table $ do
      renderReminderRow [0 .. 2] reminders
      renderReminderRow [3 .. 6] reminders
  ok val 
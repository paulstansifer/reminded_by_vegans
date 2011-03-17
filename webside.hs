module Main where

import qualified Render 
import Veganism

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Happstack.Server (nullConf, simpleHTTP, ok, notFound, dir, FromReqURI(..), path, ServerPart, Response, toResponse, Browsing(DisableBrowsing), serveDirectory)
import Happstack.Server.Monads (ServerPartT)



import Control.Monad
import Control.Monad.Trans (liftIO)

import Maybe (listToMaybe)




{- Query-related things -}


uidByEmail :: Connection -> String -> IO (Maybe UID)
uidByEmail c email = do
  r <- quickQuery' c "SELECT uid FROM usr WHERE email=?" [toSql (email :: String)]
  return $ listToMaybe $ map (UID . fromSql . head) r
  
getReminder [rid, uid, body, when, sent] = 
  Reminder (RID $ fromSql rid) (UID $ fromSql uid) (fromSql body) (fromSql when) (fromSql sent)

upcomingRemindersByUid :: Connection -> UID -> IO [Reminder]
upcomingRemindersByUid c (UID uidInt)  = do  
  timeHere <- getZonedTime
  r <- quickQuery' c "SELECT rid, uid, message, when_to_send, sent FROM reminder WHERE uid=? AND NOT sent" [toSql uidInt]
  return $ map getReminder r



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
    Nothing -> notFound $ toResponse $ Render.error "No such user" "That user doesn't exist."
    Just uid -> fn uid $ c

main :: IO ()
main = do
  print "ready"
  simpleHTTP nullConf $ msum
    [ mzero
    , userPage "reminders" remindersPage -- \(Username name) -> ok $ "Hi, " ++ name
    , dir "static" $ serveDirectory DisableBrowsing [] "./static"
      --dir "user" $ path $ \(Username name) -> ok $ "Hi" ++ name
      --dir "reminders_by_date" $ ok "Reminders by date",
      --dir "other" $ ok "Other" 
    ]

 
remindersPage :: UID -> Connection -> ServerPartT IO Response
remindersPage uid c = do
  val <- liftIO $ do 
    now <- getZonedTime -- TODO: respect user's timezone
    let today = dayOf now
    reminders <- upcomingRemindersByUid c uid
    return $ toResponse $ Render.remindersPage today reminders
  ok val 

  
  
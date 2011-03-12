import qualified Control.Exception as E
import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import qualified Text.Regex as RE
import Data.Time

--import Happstack.Util.Mail

isoDate = RE.mkRegex "([0-9]{4})[-]([0-9]{2})[-]([0-9]{2})"


listToMaybe :: [a] -> Maybe a
listToMaybe [x] = Just x
listToMaybe [] = Nothing

--f :: ZonedTime -> String
--f t = t

main =
  --E.catchDyn (do
  do
    c <- connectPostgreSQL "host=localhost dbname=rbv user=vegans password=local_login_password"

--    state <- prepare c "INSERT INTO testtable values (?,?);"
--    execute state [toSql "muhmuh", toSql (40::Int)]    
    
    let readMail =
          withTransaction c (
            \c -> do
              msg <- (quickQuery' c "SELECT * FROM inbox LIMIT 1" [])
              case msg of
                [] -> return []
                [[mid, subject, from, to, nominalTime, auto, receivedTime, headers, body]] -> do
                  case RE.matchRegexAll isoDate ((fromSql body) :: String) of
                    Nothing -> return []
                    Just (_, _, _, [y, m, d]) -> do
                      uidResponse <- (quickQuery' c "SELECT uid FROM usr WHERE email=?" [from])
                      case uidResponse of
                        [] -> do putStrLn "no user" ; return []
                        [[uid]] -> do
                          quickQuery' c "INSERT INTO reminder (uid, message, when_to_send, sent) VALUES (?, ?, ?, ?)" [uid, subject, date, toSql False]
                            where date = toSql (ZonedTime (LocalTime (fromGregorian (read y) (read m) (read d)) midnight) (hoursToTimeZone 5))
--                            where date = toSql (UTCTime (fromGregorian (read y) (read m) (read d)) (secondsToDiffTime 0))
                  quickQuery' c "DELETE FROM inbox WHERE mid=?" [mid]
                  readMail
            )
    readMail
    
    {-
    let sendReminders = 
          withTransaction c (
            \c -> do
              reminder <- (quickQuery' c "SELECT rid, message, email FROM reminder LEFT JOIN usr ON (reminder.uid = usr.uid) WHERE when_to_send < NOW() AND sent=False LIMIT 1" [])
              case reminder of
                [] -> return []
                [[rid, message, emailAddress]] -> do 
                  sendSimpleMessages "127.0.0.1" "september.endoftheinternet.org"
                    [SimpleMessage
                     [NameAddr (Just "The Vegans") "rbv@september.endoftheinternet.org"]
                     [NameAddr (Nothing) (fromSql emailAddress)]
                     "TODO"
                     (fromSql message)]
                  quickQuery' c "DELETE FROM reminder WHERE rid=?" [rid]
                  sendReminders)
    sendReminders
    -}

    disconnect c
    
    return ()
  --)
  --handler where 
  --      handler :: SqlError -> IO ()
  --      handler err = print $ "Oh no: " ++ show err
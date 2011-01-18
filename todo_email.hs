import qualified Control.Exception as E
import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import qualified Text.Regex as RE
--import qualified Data.Time.LocalTime as LT
import Data.Time
--import Data.Time.Calendar



isoDate = RE.mkRegex "([0-9]{4})[-]([0-9]{2})[-]([0-9]{2})"


-- processMsg :: [SqlValue] -> (SqlValue, Maybe String)
-- processMsg  =
--   (mid,
--    do  -- under Maybe
--      (s1, s2, s3, [y, m, d]) <- 
--      return (y ++ m ++ d)
--   )

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

-- there are various ways to get results from an executed statement. We use the lazy fetchAllRows, which works like hGetContents. Refer to the API for other functions. Note that the statement magically remembers it's been executed and carries the results around


    
    --getMsg <- prepare c "SELECT * FROM inbox LIMIT 1;"
    --execute getMsg []
    --msg <- (liftM head) (fetchAllRows getMsg)
    
    
    let read_mail =
          withTransaction c (
            \c -> do
              msg <- (quickQuery' c "SELECT * FROM inbox LIMIT 1" [])
              case msg of
                [] -> return []
                [[mid, subject, from, to, nominal_time, auto, received_time, headers, body]] -> do
                  case RE.matchRegexAll isoDate ((fromSql body) :: String) of
                    Nothing -> return []
                    Just (_, _, _, [y, m, d]) -> do
                      uid_response <- (quickQuery' c "SELECT uid FROM usr WHERE email=?" [from])
                      case uid_response of
                        [] -> do putStrLn "no user" ; return []
                        [[uid]] -> do
                          putStrLn (show date)
                          quickQuery' c "INSERT INTO reminder (uid, message, when_to_send, sent) VALUES (?, ?, ?)" [uid, subject, date, toSql False]
--                            where date = (toSql :: (ZonedTime -> SqlValue)) ((ZonedTime (LocalTime (fromGregorian (read y) (read m) (read d)) midnight) (hoursToTimeZone 5)) :: ZonedTime)
                            where date = toSql (UTCTime (fromGregorian (read y) (read m) (read d)) (secondsToDiffTime 0))
                  quickQuery' c "DELETE FROM inbox WHERE mid=?" [mid]
                  read_mail




{-
              case msgs of 
                [] -> do
                  
                  putStrLn $ show (y ++ "-" ++ m ++ "-" ++ d)
                  [[uid]] <- 
                  
                  
                  quickQuery' c "INSERT INTO reminder VALUES (?, ?, ?)" [subject, date, toSql False]
                  quickQuery' c "DELETE FROM inbox WHERE mid=?" [mid]
                  
                  read_mail
                [] -> return () -}
            )
    read_mail
    {-
    withTransaction c (
      \c -> do
        msg <- (quickQuery' c "SELECT * FROM inbox LIMIT 1;" [])
        mapM_ (\ msg -> putStrLn $ show (processMsg msg)) msgs
        
        
        )
    -}
    commit c
    disconnect c
    return ()
  --)

  --handler where 
  --      handler :: SqlError -> IO ()
  --      handler err = print $ "Oh no: " ++ show err
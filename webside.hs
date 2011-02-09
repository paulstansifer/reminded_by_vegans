module Main where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, FromReqURI(..), path)


import Control.Monad
import Control.Monad.Trans (liftIO)


data Username = Username String


instance FromReqURI Username where
  fromReqURI name = Just $ Username name

userPage pageName fn = dir "user" $ path $ \name -> dir pageName $ (fn (Username name))


main :: IO ()
main = do
  simpleHTTP nullConf $ msum
    [ mzero,
      userPage "reminders" remindersPage -- \(Username name) -> ok $ "Hi, " ++ name
      --dir "user" $ path $ \(Username name) -> ok $ "Hi" ++ name
      --dir "reminders_by_date" $ ok "Reminders by date",
      --dir "other" $ ok "Other" 
    ]



remindersPage (Username name)  = do
  val <- liftIO $ do
    c <- connectPostgresSQL "host=localhost dbname=rbv user=vegans password=local_login_password"
    
    print "hi!"
    print "hihi!"
    quickQuery' c "SELECT * FROM usr" []
  ok $ "Hi, " ++ name ++ "<br>" ++ val
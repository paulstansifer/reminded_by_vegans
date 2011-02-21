{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)
import Happstack.Server (nullConf, simpleHTTP, ok, dir, FromReqURI(..), path, ServerPart, Response, toResponse)
import Happstack.Server.SimpleHTTP (ServerPartT)

import Control.Monad
import Control.Monad.Trans (liftIO)



import           Text.Blaze ((!), toHtml)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A


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


instance FromReqURI Username where
  fromReqURI name = Just $ Username name


userPage :: String -> (Int -> Connection -> ServerPartT IO Response) -> ServerPartT IO Response
userPage pageName fn = dir "user" $ path $ \name -> dir pageName $ do
  (c, uid) <- liftIO $ do
    c <- connectPostgreSQL "host=localhost dbname=rbv user=vegans password=local_login_password"
    v <- quickQuery' c "SELECT uid FROM usr WHERE email=?" [toSql (name :: String)]
    print $ show v
    [[uid]] <- quickQuery' c "SELECT uid FROM usr WHERE email=?" [toSql (name :: String)]
    
    return (c, fromSql uid)
  
  fn uid $ c

main :: IO ()
main =
  simpleHTTP nullConf $ msum
    [ mzero,
      userPage "reminders" remindersPage -- \(Username name) -> ok $ "Hi, " ++ name
      --dir "user" $ path $ \(Username name) -> ok $ "Hi" ++ name
      --dir "reminders_by_date" $ ok "Reminders by date",
      --dir "other" $ ok "Other" 
    ]




remindersPage :: Int -> Connection -> ServerPartT IO Response
remindersPage uid c = do
  val <- liftIO $ do
    
    print "hi!"
    v <- quickQuery' c "SELECT * FROM reminder WHERE uid=?" [toSql uid]
    return $ toResponse $ appTemplate "Reminders" $ H.p $ toHtml $ show v



  ok val 
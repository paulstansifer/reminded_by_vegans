{-# LANGUAGE OverloadedStrings #-}
module Render where

import Veganism

import           Text.Blaze ((!), toHtml, stringValue, AttributeValue)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

-- pimport Data.Time
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

import Control.Monad


importCSS url = H.link ! A.rel "stylesheet" ! A.href url
importJS url = H.script ! A.type_ "text/javascript" ! A.src url $ H.string ""



page :: String  -> H.Html -> H.Html
page title body =
  H.html $ do
    H.head $ do
      H.title (H.string $ title ++ " - Reminded by Vegans")
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      importCSS "/static/march.css"
      importJS "https://www.google.com/jsapi?key=ABQIAAAAugQHkDS1r_GQH5mbj-wtLRSQiNhlh3QUmzeAsaV2bz2kQcXpqBQZU1aVzx5kI4diF8qD_VeADyZslg"
      importJS "https://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"
      importJS "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.10/jquery-ui.min.js"
      importJS "/static/provost.js"
      H.script $ H.string ""
    H.body $ do
      body
      
error :: String -> String -> H.Html
error title body = page title (H.p $ H.string body)

reminderRow :: [Day] -> [Reminder] -> H.Html
reminderRow range reminders = 
  H.table ! (A.style "width: 100%;") $ H.tr $ forM_ range $ \day ->
    H.td ! (A.class_ "day") ! (A.style $ stringValue ("width: " ++ show (100 `div` (length range)) ++ "%"))$ do
      H.b $ H.string $ formatTime defaultTimeLocale "%A the %e" day
      H.ul ! (A.class_ "remlist") $ do
        forM_ (filter (\(Reminder _ _ _ when sent) -> (not sent) && dayOf when == day) reminders) $
          \(Reminder rid _ body when sent) -> 
            H.li ! (A.class_ "rem") ! (A.id $ stringValue ("rem" ++ show rid)) $ do
              H.string "☵"
              H.form ! (A.action "javascript:alert('not yet');") $ H.input ! (A.type_ "text") 
                ! (A.value $ stringValue body)
        H.li ! (A.class_ "empty-rem") $ do
            H.string "☵"
            H.form ! (A.action "javascript:alert('not yet');") $ H.input ! (A.type_ "text")
 

remindersPage :: Day -> [Reminder] -> H.Html
remindersPage today reminders = 
  page "Reminders" $ do
    reminderRow [today .. addDays 3 today] reminders
    reminderRow [addDays 4 today .. addDays 8 today] reminders

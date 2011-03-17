module Veganism (
  Username(..), UID(..), RID(..), Reminder(..), dayOf, daysDiffThere,
  
  Day, ZonedTime, LocalTime, getZonedTime, addDays) where

import Data.Time


newtype Username = Username String
newtype UID = UID Int
              deriving (Show)
newtype RID = RID Int
              deriving (Show)

data Reminder = Reminder RID UID {-String-} String ZonedTime Bool 
              deriving (Show)

dayOf :: ZonedTime -> Day
dayOf (ZonedTime (LocalTime d t) tz) = d

daysDiffThere :: ZonedTime -> ZonedTime -> Integer
daysDiffThere timeHere (ZonedTime (LocalTime d t) there) = 
  let now = zonedTimeToUTC timeHere
      ZonedTime (LocalTime dayThere timeThere) _ = utcToZonedTime there now
  in diffDays d dayThere

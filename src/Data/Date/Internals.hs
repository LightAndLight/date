module Data.Date.Internals(
    Date
    , mkDate
    , difference
    , day
    , month
    , year
    , dayName
    , dayOrdinal
    , monthName
) where

import Data.Time (diffDays,fromGregorian,toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

data Date = Date { 
    year :: Integer -- ^ Get the year from a Date object
    , month :: Int  -- ^ Get the month from a Date object
    , day :: Int    -- ^ Get the day from a Date object
} deriving (Show, Eq, Ord)

-- | Returns a date if the supplied month and day are valid.
mkDate :: Integer -- ^ year
       -> Int     -- ^ month
       -> Int     -- ^ day
       -> Maybe Date
mkDate y m d = 
    if validDate y m d
        then Just $ Date { year = y, month = m, day = d} 
        else Nothing

-- | Computes the absolute number of days between two Date objects.
difference :: Date -> Date -> Integer
difference d1 d2 = abs $ diffDays d1' d2'
    where d1' = fromGregorian (year d1) (month d1) (day d1)
          d2' = fromGregorian (year d2) (month d2) (day d2)

validDate :: Integer -> Int -> Int -> Bool
validDate y m d = validMonth && validDay
    where validMonth = (m >= 1) && (m <= 12) 
          validDay = (d >= 1) && (d <= daysInMonth m (isLeapYear y))

daysInMonth :: Int -> Bool -> Int
daysInMonth m ly = case m of
    1  -> 31
    2  -> if ly then 29 else 28
    3  -> 31
    4  -> 30
    5  -> 31
    6  -> 30
    7  -> 31
    8  -> 31
    9  -> 30
    10 -> 31
    11 -> 30
    12 -> 31

isLeapYear :: Integer -> Bool
isLeapYear y = 
    if div400
        then True
        else if div100
            then False
            else if div4
                then True
                else False

    where div4 = y `divides` 4
          div100 = y `divides` 100
          div400 = y `divides` 400
          divides x y = (x `mod` y) == 0

daysInYear :: Integer -> Int
daysInYear y = if isLeapYear y then 366 else 365

monthName :: Int -> String
monthName m = case m of
    1  -> "January"
    2  -> "February"
    3  -> "March"
    4  -> "April"
    5  -> "May"
    6  -> "June"
    7  -> "July"
    8  -> "August"
    9  -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"

dayName :: Date -> String
dayName d = 
    let (_,_,dn) = toWeekDate $ fromGregorian (year d) (month d) (day d)
    in case dn of
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        7 -> "Sunday"

dayOrdinal :: Int -> String
dayOrdinal d = (++) (show d) $ case d of
    1  -> "st"
    2  -> "nd"
    3  -> "rd"
    21 -> "st"
    22 -> "nd"
    23 -> "rd"
    31 -> "st"
    _  -> "th"


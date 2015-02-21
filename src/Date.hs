module Data.Date(
    Date
    , mkDate
    , showDate
    , difference
) where

import Control.Exception (assert)
import Data.Time (fromGregorian,toGregorian,diffDays)
import Text.Parsec

type Day = Int
type Month = Int
type Year = Integer

data Date = Date { year :: Year, month :: Month, day :: Day }
    deriving (Show, Eq, Ord)

assertMonth :: Month -> a -> a
assertMonth m = assert ((m >= 1) && (m <= 12))

assertDay :: Year -> Month -> Day -> a -> a
assertDay y m d = assert ((d >= 1) && (d <= daysInMonth m (isLeapYear y)))

daysInMonth :: Month -> Bool -> Int
daysInMonth m ly = assertMonth m $ case m of
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

daysInYear :: Year -> Int
daysInYear y = if isLeapYear y then 366 else 365

isLeapYear :: Year -> Bool
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

mkDate :: Year -> Month -> Day -> Date
mkDate y m d = assertDay y m d $ Date { year = y, month = m, day = d}

difference :: Date -> Date -> Integer
difference d1 d2 = abs $ diffDays d1' d2'
    where d1' = fromGregorian (year d1) (month d1) (day d1)
          d2' = fromGregorian (year d2) (month d2) (day d2)

data Token =
      DayNumber
    | DayName
    | MonthNumber
    | MonthName
    | YearShort
    | YearLong
    | Literal Char
    deriving Show

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize (x:xs) = 
    let y = head xs in
    case x of
        '%' -> case y of
            'd' -> tokenize' DayNumber
            'D' -> tokenize' DayName
            'm' -> tokenize' MonthNumber
            'M' -> tokenize' MonthName
            'y' -> tokenize' YearShort
            'Y' -> tokenize' YearLong
            _   -> e x y

        '\\' -> case y of
            '%'  -> tokenize' $ Literal '%'
            '\\' -> tokenize' $ Literal '\\'
            _    -> e x y

        _   -> fmap ((Literal x):) (tokenize xs)

    where tokenize' t = fmap (t:) $ tokenize (tail xs)
          e x y = Left $ "Invalid character sequence " ++ [x,y] 

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

showToken :: Date -> Token -> String
showToken d t =
    case t of
        DayNumber   -> show $ day d
        DayName     -> show $ day d 
        MonthNumber -> show $ month d
        MonthName   -> show $ month d
        YearShort   -> takeLast 2 . show $ year d
        YearLong    -> show $ year d
        Literal c   -> return c

showDate :: String -> Date -> Either String String
showDate s d = 
    case tokenize s of
        Right ts -> Right $ concatMap (showToken d) ts
        Left e   -> Left e

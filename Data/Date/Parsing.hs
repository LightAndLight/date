module Data.Date.Parsing(showDate) where

import Data.Date.Internals

import Text.Parsec hiding (token,tokens)
import Text.Parsec.String

-- | showDate takes a format string and a Date object
-- The format string can contain the following substitution
-- identifiers:
--      * %d - day number
--      * %D - day name
--      * %o - ordinal day number
--      * %m - month number
--      * %M - month name
--      * %y - last two digits of year
--      * %Y - full year
--
-- To write literal charaters '%' and '\\', escape them double 
-- backslashes ("\\%" and "\\\\")
--
-- All other symbols will carry over as is
--
-- Usage example:
--
-- >>> showDate "%D, %o of %M, %Y" $ Date { year = 2015, month = 2, day = 22 }
-- Right "Sunday, 22nd of February, 2015"
showDate :: String -> Date -> Either ParseError String
showDate s d = 
    case parseTokens s  of
        Right ts -> Right $ concatMap (showToken d) ts
        Left e   -> Left e

data Token =
      DayNumber
    | DayOrdinal
    | DayName
    | MonthNumber
    | MonthName
    | YearShort
    | YearLong
    | Literal String
    deriving Show


identifier :: Parser Token
identifier = do
    char '%'
    next <- oneOf "dDomMyY"
    return $ case next of
        'd' -> DayNumber
        'D' -> DayName
        'o' -> DayOrdinal
        'm' -> MonthNumber
        'M' -> MonthName
        'y' -> YearShort
        'Y' -> YearLong

escaped :: Parser Char
escaped = do
    char '\\'
    next <- oneOf "%\\"
    return next

literal :: Parser Token
literal = do
    str <- many1 $ (escaped <|> noneOf "\\%")
    return $ Literal str

token :: Parser Token
token = identifier <|> literal

tokens :: Parser [Token]
tokens = many token

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

parseTokens :: String -> Either ParseError [Token]
parseTokens = parse tokens "Data.Date.Parsing"

showToken :: Date -> Token -> String
showToken d t =
    case t of
        DayNumber   -> show $ day d
        DayName     -> dayName d 
        DayOrdinal  -> dayOrdinal $ day d
        MonthNumber -> show $ month d
        MonthName   -> monthName $ month d
        YearShort   -> takeLast 2 . show $ year d
        YearLong    -> show $ year d
        Literal str   -> str

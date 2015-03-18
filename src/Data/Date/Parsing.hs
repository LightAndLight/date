module Data.Date.Parsing(
    readDate
    , showDate
) where

import Data.Date.Internals

import Data.Char (toLower)
import Text.Parsec hiding (token,tokens)
import Text.Parsec.String

-- | Takes a format string and a Date object
--
-- The format string can contain the following substitution
-- identifiers:
--
--      * %d - day number
--      * %D - day name
--      * %o - ordinal day number
--      * %m - month number
--      * %M - month name
--      * %y - last two digits of year
--      * %Y - full year
--
-- To write literal charaters '%' and '\\', escape them double 
-- backslashes ('\\%' and '\\\\')
--
-- All other symbols will carry over as is
--
-- Example:
--
-- >>> showDate "%D, %o of %M, %Y" $ Date { year = 2015, month = 2, day = 22 }
-- Right "Sunday, 22nd of February, 2015"
showDate :: String -> Date -> Either ParseError String
showDate fmt d = 
    case tokenize fmt  of
        Right ts -> Right $ concatMap (showToken d) ts
        Left e   -> Left e

readDate :: String -> String -> Either ParseError (Maybe Date)
readDate fmt s =
    case tokenize fmt of
        Right ts -> parseDate ts s
        Left e   -> Left e

data DateState = DateState { 
    yearState :: Maybe Integer
    , monthState :: Maybe Int
    , dayState :: Maybe Int
}

readToken :: Token -> DateState -> Parser DateState
readToken (DayNumber) ds = do
    d <- try (count 2 digit) <|> count 1 digit
    return ds { dayState = Just (read d) }

readToken (DayOrdinal) ds = do
    d <- try (count 2 digit) <|> count 1 digit
    string "st" <|> string "nd" <|> string "rd" <|> string "th"
    return ds { dayState = Just (read d) }

readToken (MonthNumber) ds = do
    m <- try (count 2 digit) <|> count 1 digit
    return ds { monthState = Just (read m) }

readToken (MonthName) ds = do
    m <- many1 letter
    return ds { monthState = case map toLower m of
        "january"   -> Just 1
        "february"  -> Just 2
        "march"     -> Just 3
        "april"     -> Just 4
        "may"       -> Just 5
        "june"      -> Just 6
        "july"      -> Just 7
        "august"    -> Just 8
        "september" -> Just 9
        "october"   -> Just 10
        "november"  -> Just 11
        "december"  -> Just 12
        _           -> Nothing
    }
    
readToken (YearLong) ds = do
    y <- count 4 digit
    return ds { yearState = Just (read y) }

readToken (Literal s) ds = do
    string s
    return ds

readToken _ ds = return ds

parseDate :: [Token] -> String -> Either ParseError (Maybe Date)
parseDate tks s = 
    case dateState of
        Right ds -> case ds of
            DateState (Just y) (Just m) (Just d) -> Right $ mkDate y m d
            _                                    -> Right Nothing
        Left e   -> Left e
    where initial = return $ DateState { 
              yearState = Nothing
              , monthState = Nothing
              , dayState = Nothing 
          }
          pDateState = foldl (>>=) initial (map readToken tks)
          dateState = parse pDateState "Data.Date" s

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
    oneOf "%\\"

literal :: Parser Token
literal = do
    str <- many1 $ escaped <|> noneOf "\\%"
    return $ Literal str

token :: Parser Token
token = identifier <|> literal

tokens :: Parser [Token]
tokens = many token

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

tokenize :: String -> Either ParseError [Token]
tokenize = parse tokens "Data.Date.Parsing"

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

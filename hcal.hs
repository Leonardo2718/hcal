{-
Project: hcal
File: hcal.hs
Author: Leonardo Banderali
Last Modified: Febuary 12, 2016

Description:
    hcal is a simple calendar program for the terminal. It is intended to be an
    improvement on cal (the Unix calendar program).


Copyright (C) 2016 Leonardo Banderali

License:

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
-}

import System.Environment (getArgs)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.List
import Data.List.Split
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.Word
import Data.Function (on)



-- convenience functions ---------------------------------------------------------------------------

groupInto :: Integral i => i -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = genericTake n xs: groupInto n (genericDrop n xs)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

fst4 :: (a,b,c,d) -> a
fst4 (x,_,_,_) = x

snd4 :: (a,b,c,d) -> b
snd4 (_,x,_,_) = x

trd4 :: (a,b,c,d) -> c
trd4 (_,_,x,_) = x

frt4 :: (a,b,c,d) -> d
frt4 (_,_,_,x) = x



-- command line option handling --------------------------------------------------------------------

data YearOption = None | ShowCurrent | ShowYear Integer deriving (Show)

data Options = Options
    { optYear           :: YearOption               -- which year to show
    , optDayRange       :: (Maybe Day, Maybe Day)   -- range of days to display
    , optColumnCount    :: Word                     -- how many columns to show
    , optSundayWeek     :: Bool                     -- if weeks should start on Sunday instead of Monday
    , optHelp           :: Bool                     -- display usage information
    } deriving Show

defaultOptions = Options
    { optYear           = None
    , optDayRange       = (Nothing, Nothing)
    , optColumnCount    = 4
    , optSundayWeek     = False
    , optHelp           = False
    }

optionTransforms :: [OptDescr (Options -> Options)]
optionTransforms =
    [ Option ['y'] ["year"]
        (OptArg (\ s opts -> opts {optYear = readAsYearOption s}) "YEAR")
        "display calendar for whole year; optionally specify the YEAR displayed"
    , Option ['r'] ["range"]
        (ReqArg (\ s opts -> opts {optDayRange = parseDayRange s}) "DAY_RANGE")
        "display calendar with days in the range specified"
    , Option ['c'] []
        (ReqArg (\ s opts -> opts {optColumnCount = read s :: Word}) "COLUMNS")
        "display calendar with COLUMNS number of columns; COLUMNS must be a positive integer"
    , Option ['m'] ["monday"]
        (NoArg (\ opts -> opts {optSundayWeek = False}))
        "use Monday as first day of the week"
    , Option ['s'] ["sunday"]
        (NoArg (\ opts -> opts {optSundayWeek = True}))
        "use Sunday as first day of the week"
    , Option ['h'] ["help"]
        (NoArg (\ opts -> opts {optHelp = True}))
        "display this help message"
    ]
    where
        readAsYearOption Nothing  = ShowCurrent
        readAsYearOption (Just s) = ShowYear (read s :: Integer)
        parseDayRange = toRange . toTuple . splitOn ":" where
            toRange (s1,s2) = (readAsMaybeDay s1, readAsMaybeDay s2)
            toTuple xs = if length xs == 2 then (head xs, last xs) else error "Invalid Range."
            readAsMaybeDay s = if null s then Nothing else Just (readAsDay s)
            readAsDay s = fromGregorian year month date where
                year = read (ds!!0) :: Integer
                month = read (ds!!1) :: Int
                date = read (ds!!2) :: Int
                ds = splitOn "-" s

applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults =
    case getOpt Permute transforms argv of
        (o,[],[] ) -> foldl (flip id) defaults o
        (_,n,[]) -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ helpMessage)
        (_,_,errs) -> error (concat errs ++ helpMessage)

helpMessage :: String
helpMessage = usageInfo header optionTransforms ++ footer where
    header = "\nUsage: hcal [OPTION]"
    footer = "\nSee project home page for more information.\n"



-- constants for the application -------------------------------------------------------------------

monthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"]

mDayNames = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
sDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]



-- main program ------------------------------------------------------------------------------------

shortDayNames :: [String] -> [String]
shortDayNames = map (take 2)

data Range t = Range {startOf :: t, endOf :: t} deriving Show

firstDayIn (Range d _, _) = d
lastDayIn (Range _ d, _) = d

mergeDayRanges rs = (Range (firstDayIn (head rs)) (lastDayIn (last rs)), map snd rs)

centerTextIn n s = leftPadding ++ s ++ rightPadding where
    leftPadding = replicate (halfPadding - length s `mod` 2) ' '
    rightPadding = replicate halfPadding ' '
    halfPadding = n `div` 2 - length s `div` 2 -- each term must be devided separately because of integer division

daysToCalendar :: Word -> Bool -> [Day] -> String
daysToCalendar columns firstDaySunday = showAsCalendar . showAsYears . showAsRows . showAsMonths . showAsWeeks . showAsDays where
    showAsCalendar = intercalate "\n" . map snd

    showAsYears = map (unlink . pad . mergeDayRanges) . group where
        unlink (r, ss) = (r, intercalate "\n" ss)
        pad (Range d1 d2, ss) = (Range d1 d2, header ++ ss) where
            header = [centerTextIn (length . head . lines . head $ ss) (show . yearOf $ d1)]
        group = groupBy (\ (Range d1 _, _) (Range d2 _, _) -> daysInSameYear d1 d2)

    showAsRows = map (unlink . mergeDayRanges) . group where
        unlink (r, ss) = (r, intercalate "\n" . map (intercalate "  ") . transpose $ ss)
        group = groupInto columns

    showAsMonths = map (pad . mergeDayRanges) . group where
        pad (Range d1 d2, ss) = (Range d1 d2, header ++ front ++ ss ++ back) where
            header = [centerTextIn 20 (monthNames !! (monthOf d1 - 1)), dayNames] where
                dayNames = unwords . shortDayNames $ if firstDaySunday then sDayNames else mDayNames
            front = replicate (firstWeek - monthStartWeek) (replicate 20 ' ') where
                firstWeek = fst . weekOf $ d1
                monthStartWeek = fst . weekOf . (\(y,m,d) -> fromGregorian y m 1 ) . toGregorian $ d1
            back = replicate (monthEndWeek - lastWeek + 7 - weeksInMonth) (replicate 20 ' ') where -- need to account for varying number of weeks in months
                lastWeek = fst . weekOf $ d2
                monthStartWeek = fst . weekOf . (\(y,m,d) -> fromGregorian y m 1 ) . toGregorian $ d1
                monthEndWeek = fst . weekOf . (\(y,m,d) -> fromGregorian y m (gregorianMonthLength y m)) . toGregorian $ d2
                weeksInMonth = monthEndWeek - monthStartWeek + 1
        group = groupBy (\ (Range d1 _, _) (Range d2 _, _) -> daysInSameMonth d1 d2)

    showAsWeeks = map (unlink . pad . mergeDayRanges) . group where
        unlink (r, ss) = (r, unwords ss)
        pad (Range d1 d2, ss) = (Range d1 d2, front ++ ss ++ back) where
            front = replicate (dayNumber d1 - 1) "  "
            back = replicate (7 - dayNumber d2) "  "
            dayNumber = (\d -> if firstDaySunday then d + 1 else d) . snd . weekOf
        group = groupBy (\ (Range d1 _, _) (Range d2 _, _) -> daysInSameWeek firstDaySunday d1 d2 && daysInSameMonth d1 d2)

    showAsDays = map (\d -> (Range d d, showDay d)) . group where
        showDay = (\d -> if length d == 1 then ' ':d else d) . show . dateOf
        group = id

    weekOf = if firstDaySunday then sundayStartWeek else mondayStartWeek



monthWithDay :: Day -> [Day]
monthWithDay day = [(fromGregorian y m 1)..(fromGregorian y m lastDay)] where
    (y, m, d) = toGregorian day
    lastDay = gregorianMonthLength y m

yearWithDay :: Day -> [Day]
yearWithDay day = [(fromGregorian y 1 1)..(fromGregorian y 12 31)] where
    (y, _, _) = toGregorian day

yearOf :: Day -> Integer
yearOf = fst3 . toGregorian

monthOf :: Day -> Int
monthOf = snd3 . toGregorian

dateOf :: Day -> Int
dateOf = trd3 . toGregorian

daysInSameYear :: Day -> Day -> Bool
daysInSameYear day1 day2 = yearOf day1 == yearOf day2

daysInSameMonth :: Day -> Day -> Bool
daysInSameMonth day1 day2 = monthOf day1 == monthOf day2 && yearOf day1 == yearOf day2

daysInSameWeek :: Bool -> Day -> Day -> Bool
daysInSameWeek firstDaySunday day1 day2 = weekOf day1 == weekOf day2 && yearOf day1 == yearOf day2 where
    weekOf = if firstDaySunday then fst . sundayStartWeek else fst . mondayStartWeek



-- main IO -----------------------------------------------------------------------------------------

genDays :: Options -> Day -> [Day]
genDays options today   = case optDayRange options of
    (Nothing, Nothing)  -> tryCurrentYear
    (Nothing, Just d)   -> [today..d]
    (Just d, Nothing)   -> [d..today]
    (Just d1, Just d2)  -> [d1..d2]
    where
        tryCurrentYear  = case optYear options of
                            None        -> currentMonth
                            ShowCurrent -> yearWithDay today
                            ShowYear y  -> [(fromGregorian y 1 1)..(fromGregorian y 12 31)]
        currentMonth    = monthWithDay today


main :: IO ()
main = do
    args <- getArgs
    todayUTC <- getCurrentTime
    let options = applyOptionTransforms optionTransforms args defaultOptions
    todayUTC <- getCurrentTime
    let today = utctDay todayUTC
    let days = genDays options today
    putStrLn (if optHelp options then helpMessage else daysToCalendar (optColumnCount options) (optSundayWeek options) days)

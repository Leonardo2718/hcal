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
    { optYear           :: YearOption   -- which year to show
    , optColumnCount    :: Word         -- how many columns to show
    , optSundayWeek     :: Bool         -- if weeks should start on Sunday instead of Monday
    , optDayRange       :: (Maybe Day, Maybe Day)   -- range of days to display
    } deriving Show

defaultOptions = Options
    { optYear           = None
    , optColumnCount    = 4
    , optSundayWeek     = False
    , optDayRange       = (Nothing, Nothing)
    }

optionTransforms :: [OptDescr (Options -> Options)]
optionTransforms =
    [ Option ['y'] ["year"]
        (OptArg (\ s opts -> opts {optYear = readAsYearOption s}) "YEAR")
        "display calendar for whole year; optionally specify the YEAR displayed"
    , Option ['c'] []
        (ReqArg (\ s opts -> opts {optColumnCount = read s :: Word}) "COLUMNS")
        "display calendar with COLUMNS number of columns"
    , Option ['s'] ["sunday"]
        (NoArg (\ opts -> opts {optSundayWeek = True}))
        "display Sunday as first day of the week"
    , Option ['r'] ["range"]
        (ReqArg (\ s opts -> opts {optDayRange = parseDayRange s}) "DAY_RANGE")
        "display calendar with days in the range specified"
    ]
    where
        readAsYearOption Nothing  = ShowCurrent
        readAsYearOption (Just s) = ShowYear (read s :: Integer)
        parseDayRange = toRange . toTuple . splitOn ":" where
            toRange (s1,s2) = (readAsMaybeDay s1, readAsMaybeDay s2)
            toTuple xs = if length xs == 2 then (head xs, last xs) else error "Invalid Range."
            readAsMaybeDay s = if length s == 0 then Nothing else Just (readAsDay s)
            readAsDay s = fromGregorian year month date where
                year = read (ds!!0) :: Integer
                month = read (ds!!1) :: Int
                date = read (ds!!2) :: Int
                ds = splitOn "-" s

applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults =
    case getOpt Permute transforms argv of
        (o,[],[] ) -> foldl (flip id) defaults o
        (_,n,[]) -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ usageInfo header transforms)
        (_,_,errs) -> error (concat errs ++ usageInfo header transforms)
        where header = "\nUsage: hcal [OPTION...]"



-- constants for the application -------------------------------------------------------------------

monthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"]

mDayNames = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
sDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

shortDayNames :: [String] -> [String]
shortDayNames = map (take 2)



-- main program ------------------------------------------------------------------------------------

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

daysInSameMonth :: Day -> Day -> Bool
daysInSameMonth day1 day2 = monthOf day1 == monthOf day2 && yearOf day1 == yearOf day2

daysInSameWeek :: Bool -> Day -> Day -> Bool
daysInSameWeek firstDaySunday day1 day2 = weekOf day1 == weekOf day2 && yearOf day1 == yearOf day2 where
    weekOf = case firstDaySunday of
        False -> fst . mondayStartWeek
        True -> fst . sundayStartWeek

showCal :: Word -> Bool -> [Day] -> String
showCal columns firstDaySunday = showAsCalendar . monthsAsYears . monthWeeksAsMonths . daysAsMonthWeeks where
    showAsCalendar      = intercalate "\n\n" . map (intercalate "\n" . addHeader) where
        addHeader (y,ms,ws,ss)= header:ss where
            header              = leftPadding ++ year ++ rightPadding where
                year                = show y
                leftPadding         = replicate (paddingLength - length year `mod` 2) ' '
                rightPadding        = replicate paddingLength ' '
                paddingLength       = (min (read (show columns) :: Int) (length ms) * 22 - 2 - length year) `div` 2  -- cast `columns` to Int

    monthsAsYears       = map (showMonths . pullYearInfo) . groupByYears where
        padYear (y,ms,ws,ss)= (y, ms, ws, leftPadding ++ ss ++ rightPadding) where
            leftPadding         = replicate (head ms - 1)  (replicate 8 . replicate 20 $ ' ')
            rightPadding        = replicate (12 - last ms) (replicate 8 . replicate 20 $ ' ')
        showMonths (y,ms,ws,ss)= (y, ms, ws, map (intercalate "\n" . map (intercalate "  ") . transpose) . groupInto columns $ ss)
        pullYearInfo months = (fst4 (head months), map snd4 months, map trd4 months, map frt4 months)
        groupByYears        = groupBy (\ m1 m2 -> fst4 m1 == fst4 m2)

    monthWeeksAsMonths  = map (addHeader . padMonth . showWeeks . pullMonthInfo) . groupByMonth where
        addHeader (y,m,ws,ss)= (y, m, ws, header ++ ss) where
            header          = [leftPadding ++ h ++ rightPadding] ++ [dayNames]
            dayNames            = unwords (shortDayNames (if firstDaySunday then sDayNames else mDayNames))
            h                   = monthNames !! (m - 1)
            leftPadding         = replicate (10 - length h `div` 2 - length h `mod` 2) ' '
            rightPadding        = replicate (10 - length h `div` 2) ' '
        padMonth (y,m,ws,ss)= (y, m, ws, topPadding ++ ss ++ bottomPadding) where
            topPadding          = replicate (head ws - firstMonthWeek) (replicate 20 ' ')
            bottomPadding       = replicate (5 - (lastMonthWeek - firstMonthWeek) + (lastMonthWeek - last ws)) (replicate 20 ' ')
            firstMonthWeek      = fst . weekOf . fromGregorian y m $ 1
            lastMonthWeek       = fst . weekOf . fromGregorian y m . gregorianMonthLength y $ m
        showWeeks (y,m,ws,ss)= (y, m, ws, map unwords ss)
        pullMonthInfo weeks = (fst4 (head weeks), snd4 (head weeks), map trd4 weeks, map frt4 weeks)
        groupByMonth        = groupBy (\ w1 w2 -> snd4 w1 == snd4 w2)

    daysAsMonthWeeks    = map (padWeek . padDays . showDays . pullWeekInfo) . groupByWeek where
        padWeek (y,m,w,ds)  = (y, m, w, leftPadding ++ ds ++ rightPadding) where
            leftPadding         = replicate (dayNumber (head ds) - 1) "  "
            rightPadding        = replicate (7 - dayNumber (last ds)) "  "
            dayNumber d         = case firstDaySunday of
                                    False   -> snd . mondayStartWeek . fromGregorian y m $ (read d :: Int)
                                    True    -> (snd . sundayStartWeek . fromGregorian y m $ (read d :: Int)) + 1
        padDays (y,m,w,ds)  = (y, m, w, map (\d -> if length d == 1 then ' ':d else d) ds)
        showDays (y,m,w,ds) = (y, m, w, map show ds)
        pullWeekInfo days   = (yearOf (head days), monthOf (head days), fst (weekOf (head days)), map dateOf days)
        groupByWeek         = groupBy (\ d1 d2 -> daysInSameWeek firstDaySunday d1 d2 && daysInSameMonth d1 d2)

    weekOf             = if firstDaySunday then sundayStartWeek else mondayStartWeek



-- main IO -----------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    todayUTC <- getCurrentTime
    let options = applyOptionTransforms optionTransforms args defaultOptions
    todayUTC <- getCurrentTime
    let today = utctDay todayUTC
    let days = case optDayRange options of
            (Nothing, Nothing)  -> case optYear options of
                                    None        -> monthWithDay today
                                    ShowCurrent -> yearWithDay today
                                    ShowYear y  -> [(fromGregorian y 1 1)..(fromGregorian y 12 31)]
            (Nothing, Just d)   -> [today..d]
            (Just d, Nothing)   -> [d..today]
            (Just d1, Just d2)  -> [d1..d2]
    putStrLn . showCal (optColumnCount options) (optSundayWeek options) $ days

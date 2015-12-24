import System.Environment (getArgs)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.List
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.Word



-- convenience functions ---------------------------------------------------------------------------

groupInto :: Integral i => i -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = genericTake n xs: groupInto n (genericDrop n xs)



-- command line option handling --------------------------------------------------------------------

data YearOption = None | ShowCurrent | ShowYear Integer deriving (Show)

data Options = Options
    { optYear           :: YearOption   -- which year to show
    , optColumnCount    :: Word         -- how many columns to show
    , optSundayWeek     :: Bool         -- if weeks should start on Sunday instead of Monday
    } deriving Show

defaultOptions = Options
    { optYear           = None
    , optColumnCount    = 4
    , optSundayWeek     = False
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
    ]
    where
        readAsYearOption Nothing  = ShowCurrent
        readAsYearOption (Just s) = ShowYear (read s :: Integer)

applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults =
    case getOpt Permute transforms argv of
        (o,[],[] ) -> foldl (flip id) defaults o
        (_,n,[]) -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ usageInfo header transforms)
        (_,_,errs) -> error (concat errs ++ usageInfo header transforms)
        where header = "\nUsage: hcal [OPTION...]"



-- constants for the application -------------------------------------------------------------------

monthNames = ["January","February","March","April","May","June","Juli","August","September","October","November","December"]

mDayNames = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
sDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

shortDayNames :: [String] -> [String]
shortDayNames = map $ take 2



-- main program ------------------------------------------------------------------------------------

data Month = Month {yearOf :: Integer, monthNameOf :: String, weeksOf :: [[String]]} deriving (Show)

daysToMonth :: [[Day]] -> Month
daysToMonth days = Month y (monthNames !! (m-1)) (paddedDays ++ replicate (6 - length paddedDays) ["                    "]) where
    (y, m, _) = toGregorian . head . head $ days
    paddedDays = padDays (map (map showDay) days)
    padDays ds = [paddedFront] ++ middle ++ [paddedBack] where
        middle = init . tail $ ds
        front = head ds
        back = last ds
        paddedFront = replicate (7 - length front) "  " ++ front
        paddedBack = back ++ replicate (7 - length back) "  "

monthWithDay :: Day -> [Day]
monthWithDay day = [(fromGregorian y m 1)..(fromGregorian y m lastDay)] where
    (y, m, d) = toGregorian day
    lastDay = gregorianMonthLength y m

yearWithDay :: Day -> [Day]
yearWithDay day = [(fromGregorian y 1 1)..(fromGregorian y 12 31)] where
    (y, _, _) = toGregorian day

monthAsRows :: Month -> [String]
monthAsRows m = monthHeader m : showWeek (shortDayNames mDayNames) : (map showWeek . weeksOf $ m) where
    monthHeader m = lpadding ++ header ++ rpadding
    header = monthNameOf m ++ " " ++ show (yearOf m)
    lpadding = replicate (10 - length header `div` 2 - length header `mod` 2) ' ' -- need to subtract mod to compansate for extra space when length is odd
    rpadding = replicate (10 - length header `div` 2) ' '
    showWeek = unwords



showDay :: Day -> String
showDay day = padDay . show $ d where
    (_,_,d) = toGregorian day
    padDay ds = if length ds == 1 then ' ':ds else ds

showCal :: Word -> [Day] -> String
showCal c = concat . showMonths c . map (daysToMonth . groupBy daysInSameWeek) . groupBy daysInSameMonth where
    showMonths c = intersperse "\n\n" . map mergeMonths . groupInto c . map monthAsRows
    mergeMonths = intercalate "\n" . map (intercalate "   ") . transpose



daysInSameWeek :: Day -> Day -> Bool
daysInSameWeek day1 day2 = week1 == week2 && year1 == year2 where
    (year1, week1, _) = toWeekDate day1
    (year2, week2, _) = toWeekDate day2

daysInSameMonth :: Day -> Day -> Bool
daysInSameMonth day1 day2 = month1 == month2 && year1 == year2 where
    (year1, month1, _) = toGregorian day1
    (year2, month2, _) = toGregorian day2



-- main IO -----------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    todayUTC <- getCurrentTime
    let options = applyOptionTransforms optionTransforms args defaultOptions
    todayUTC <- getCurrentTime
    let days = case optYear options of
            None        -> monthWithDay . utctDay $ todayUTC
            ShowCurrent -> yearWithDay . utctDay $ todayUTC
            ShowYear y  -> [(fromGregorian y 1 1)..(fromGregorian y 12 31)]
    putStrLn . showCal (optColumnCount options) $ days

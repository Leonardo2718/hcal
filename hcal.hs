import System.Environment (getArgs)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.List
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )



-- convenience functions ---------------------------------------------------------------------------

groupInto :: Integral i => i -> [a] -> [[a]]
groupInto _ [] = []
groupInto n xs = genericTake n xs: groupInto n (genericDrop n xs)



-- command line option handling --------------------------------------------------------------------

data Options = Options
    { optYear           :: Maybe Integer
    , optColumnCount    :: Int  -- maybe should use Data.Word instead (for unsigned type)
    } deriving Show

defaultOptions = Options
    { optYear           = Nothing
    , optColumnCount    = 4
    }

optionTransforms :: Day -> [OptDescr (Options -> Options)]
optionTransforms today = 
    [ Option ['y'] ["year"]
        (OptArg (\ s opts -> opts {optYear = Just (readAsYear s)}) "YEAR")
        "display calendar for whole year; optionally specify the YEAR displayed"
    , Option ['c'] []
        (ReqArg (\ s opts -> opts {optColumnCount = read s :: Int}) "COLUMNS")
        "display calendar with COLUMNS number of columns"
    ]
    where
        readAsYear Nothing  = y where (y,_,_) = toGregorian today
        readAsYear (Just s) = read s :: Integer

applyOptionTransforms :: [OptDescr (Options -> Options)] -> [String] -> Options -> Options
applyOptionTransforms transforms argv defaults = 
    case getOpt Permute transforms argv of
        (o,[],[] ) -> foldl (flip id) defaults o
        (_,n,[]) -> error (concatMap (\ a -> "Unknown argument: " ++ a ++ "\n") n ++ usageInfo header transforms)
        (_,_,errs) -> error (concat errs ++ usageInfo header transforms)
        where header = "\nUsage: hcal [OPTION...]"


        
-- constants for the application -------------------------------------------------------------------
        
monthNames = ["January","February","March","April","May","June","Juli","August","September","October","November","December"]

shortWeekNames = ["Mo","Tu","We","Th","Fr","Sa","Su"]



-- main program ------------------------------------------------------------------------------------

data Month = Month {yearOf :: Integer, monthNameOf :: String, weeksOf :: [[String]]} deriving (Show)

daysToMonth :: [[Day]] -> Month
daysToMonth days = Month y (monthNames !! (m-1)) (paddedDays ++ (replicate (6 - length paddedDays) ["                    "])) where
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

monthAsRows :: Month -> [String]
monthAsRows m = monthHeader m : showWeek shortWeekNames : (map showWeek . weeksOf $ m) where
    monthHeader m = padding ++ header ++ padding
    header = monthNameOf m ++ " " ++ show (yearOf m)
    padding = replicate (10 - length header `div` 2) ' '
    showWeek = concat . intersperse " " -- use unwords



showDay :: Day -> String
showDay day = padDay . show $ d where
    (_,_,d) = toGregorian day
    padDay ds = if length ds == 1 then ' ':ds else ds

showCal :: Int -> [Day] -> String
showCal c = concat . showMonths c . map daysToMonth . map (groupBy daysInSameWeek) . groupBy daysInSameMonth where
    showMonths c = intersperse "\n\n" . map mergeMonths . groupInto c . map monthAsRows
    mergeMonths = concat . intersperse "\n" . map concat . map (intersperse "   ") . transpose



daysInSameWeek :: Day -> Day -> Bool
daysInSameWeek day1 day2 = week1 == week2 && year1 == year2 where
    (year1, week1, _) = toWeekDate day1
    (year2, week2, _) = toWeekDate day2

daysInSameMonth :: Day -> Day -> Bool
daysInSameMonth day1 day2 = month1 == month2 && year1 == year2 where
    (year1, month1, _) = toGregorian day1
    (year2, month2, _) = toGregorian day2



parseArgs :: [String] -> IO ()

parseArgs [] = getCurrentTime >>= return . showCal 4 . monthWithDay . utctDay >>= putStrLn

parseArgs ["-y"]  = parseArgs ["-y", "-c", "4"]
parseArgs ["-y", "-c", c] = getCurrentTime >>= return . showCal columns . yearWithDay . utctDay >>= putStrLn where
    yearWithDay d = [(fromGregorian y 1 1)..(fromGregorian y 12 31)] where (y, _, _) = toGregorian d
    columns = read c :: Int

parseArgs ["-y", y] = parseArgs ["-y", y, "-c", "4"]
parseArgs ["-y", y, "-c", c] = (return . showCal columns $ [(fromGregorian year 1 1)..(fromGregorian year 12 31)]) >>= putStrLn where
    year    = read y :: Integer
    columns = read c :: Int

parseArgs xs = do
    print "Error: "
    print xs



-- main IO -----------------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    todayUTC <- getCurrentTime
    let options = applyOptionTransforms (optionTransforms . utctDay $ todayUTC) args defaultOptions
    print options

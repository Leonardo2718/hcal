module Main where

import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock

type DateRange = (Day, Day)

data Options = Options { dateRange :: DateRange
                       , today :: Day
                       }

-- utility functions ----------------------------------------------------------

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thr3 (_,_,z) = z

groupsOf _ [] = []
groupsOf n ls = take n ls : groupsOf n (drop n ls)

relation :: (b -> b -> c) -> (a -> b) -> a -> a -> c
relation rel f = (\ x y -> f x `rel` f y)

-- like `Prelude.unlines` but does not add a new line at the very end
myUnlines = intercalate "\n"

getYear  = fst3 . toGregorian
getMonth = snd3 . toGregorian
getDay   = thr3 . toGregorian
getWeek  = snd3 . toWeekDate

withYear y date  = fromGregorian y (getMonth date) (getDay date)
withMonth m date = fromGregorian (getYear date) m (getDay date)
withDay d date   = fromGregorian (getYear date) (getMonth date) d

daysInRange a b = [a..b]

leftPad n e ls = (replicate (n - length ls) e) ++ ls
rightPad n e ls = ls ++ (replicate (n - length ls) e)
centerPad n e ls =
    let padding = n - length ls
        lpad = padding `div` 2
        rpad = lpad + padding `rem` 2
    in (replicate lpad e) ++ ls ++ (replicate rpad e)
padLists n e lss = leftPad n e (head lss) : padLast (tail lss)
    where padLast [] = []
          padLast [ls] = [rightPad n e ls]
          padLast (ls:lss') = ls : padLast lss'

data Month = January | Febuary | March | April | May | June | July | August | September | October | November | December deriving (Eq, Show, Enum)
toMonth :: Int -> Month
toMonth = toEnum . (\x -> x - 1)

weekPrefixes :: String
weekPrefixes = unwords . map (take 2 . show) $ [Monday .. Sunday]

-- main program ---------------------------------------------------------------

makeCalendar :: Options -> String
makeCalendar options = let
    asMonthRows month =
        let monthName = centerPad (length weekPrefixes) ' ' . show . toMonth . getMonth . (!! 0) $ month
            showDay = leftPad 2 ' ' . show . getDay
            weeks = padLists (length weekPrefixes) ' ' . map (unwords . map showDay) . groupBy (relation (==) getWeek) $ month
        in  monthName : weekPrefixes : weeks
    showMonths months = myUnlines . foldr1 (zipWith (\a b -> a ++ "  " ++ b)) . map asMonthRows $  months
    showYear days = myUnlines . map showMonths . groupsOf 3 . groupBy (relation (==) getMonth) $ days
    showCalendar days = myUnlines . map showYear . groupBy (relation (==) getYear) $ days
    in showCalendar . uncurry daysInRange $ dateRange options

main :: IO ()
main = do
    today <- getCurrentTime >>= return . utctDay
    putStrLn $ makeCalendar Options{ dateRange = (withDay 1 . withMonth 1 $ today, withDay 31 . withMonth 12 $ today), today = today}

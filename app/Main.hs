module Main where

import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock

-- utility functions ----------------------------------------------------------

type DateRange = (Day, Day)

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

groupsOf _ [] = []
groupsOf n ls = take n ls : groupsOf n (drop n ls)

relation :: (b -> b -> c) -> (a -> b) -> a -> a -> c
relation rel f = (\ x y -> f x `rel` f y)

-- like `Prelude.unlines` but does not add a new line at the very end
myUnlines = intercalate "\n"

getYear  = fst3 . toGregorian
getMonth = snd3 . toGregorian
getDay   = thd3 . toGregorian
getWeek  = snd3 . toWeekDate

withYear y date  = fromGregorian y (getMonth date) (getDay date)
withMonth m date = fromGregorian (getYear date) m (getDay date)
withDay d date   = fromGregorian (getYear date) (getMonth date) d

daysInRange a b = [a..b]

prePad n e ls = (replicate (n - length ls) e) ++ ls
postPad n e ls = ls ++ (replicate (n - length ls) e)
centerPad n e ls =
    let padding = n - length ls
        lpad = padding `div` 2
        rpad = lpad + padding `rem` 2
    in (replicate lpad e) ++ ls ++ (replicate rpad e)
padLists n e lss = prePad n e (head lss) : padLast (tail lss)
    where padLast [] = []
          padLast [ls] = [postPad n e ls]
          padLast (ls:lss') = ls : padLast lss'

data Month = January | Febuary | March | April | May | June | July | August | September | October | November | December deriving (Eq, Show, Enum)
toMonth :: Int -> Month
toMonth = toEnum . (\x -> x - 1)

weekPrefixes :: String
weekPrefixes = unwords . map (take 2 . show) $ [Monday .. Sunday]

-------------------------------------------------------------------------------

longestRowLength = maximum . map length

rowFor :: a -> [[a]] -> [a]
rowFor e lss = replicate (longestRowLength lss) e

tabulate :: (Semigroup a) => a -> [[[a]]] -> [a]
tabulate sep = map (foldl1 (<>) . intersperse sep) . concatMap transpose

tabulateMap :: (Semigroup b) => b -> ([a] -> [b]) -> [[[a]]] -> [b]
--tabulateMap sep f = concatMap (foldr1 (zipWith (\a b -> a <> sep <> b)) . map f)
tabulateMap sep f = map (foldr1 (<>) . intersperse sep) . concatMap (transpose . map f)

asTableRows :: (Eq s) => Int -> String -> (a -> s) -> ([a] -> [String]) -> [a] -> [String]
asTableRows cols sep selector f = tabulateMap sep f . groupsOf cols . groupBy (relation (==) selector)

-- main program ---------------------------------------------------------------

data Options = Options { dateRange :: DateRange
                       , today :: Day
                       }

monthAsRows :: [Day] -> [String]
monthAsRows month =
    let monthName = centerPad (length weekPrefixes) ' ' . show . toMonth . getMonth . (!! 0) $ month
        showDay   = prePad 2 ' ' . show . getDay
        weeks     = padLists (length weekPrefixes) ' ' . map (unwords . map showDay) . groupBy (relation (==) getWeek) $ month
    in  monthName : weekPrefixes : postPad 6 (rowFor ' ' weeks) weeks

yearAsRows :: [Day] -> [String]
yearAsRows year =
    let rows    = asTableRows 3 "  " getMonth monthAsRows year
        yearStr = centerPad (longestRowLength rows) ' ' . show . getYear . (!! 0) $ year
    in yearStr : rows


showCalendar :: [Day] -> String
showCalendar = myUnlines . asTableRows 2 "   " getYear yearAsRows

makeCalendar :: Options -> String
makeCalendar options = showCalendar . uncurry daysInRange $ dateRange options

main :: IO ()
main = do
    today <- getCurrentTime >>= return . utctDay
    putStrLn $ makeCalendar Options{ dateRange = (withDay 1 . withMonth 1 $ today, withDay 31 . withMonth 12 . (\d -> withYear (getYear d + 3) d ) $ today), today = today}

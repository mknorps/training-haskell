-- Write 11 functions to compare dates
-- Date is represented with three integers
module Excercise1
    ( someFunc,
      is_older,
      number_in_month,
      number_in_months,
      dates_in_month,
      dates_in_months,
      get_nth,
      number_before_reaching_sum,
      what_month,
      month_range,
      oldest,
      MyDate(..) -- to export constructor & methods as well 
    ) where


import qualified Data.Set as S

type D = MyDate
data MyDate = D
    { 
      year :: Integer,
      month :: Integer,
      day :: Integer
    }

months = ["January", "February", "March", "April", "May", "June",
          "July", "August", "September", "October", "November", "December"]

instance Show MyDate where
    show d = extractString (get_nth months (month d)) ++ " " ++
        show (day d) ++ ", " ++
        show (year d)

instance Eq MyDate where
    x == y = day x == day y &&
             month x == month y &&
             year x == year y

extractString :: Maybe String -> String
extractString mx = case mx of
    Nothing -> ""
    Just x -> x


someFunc :: IO ()
someFunc = print $ day $D 2018 12 24

-- 1) check if the first date is older
is_older :: MyDate -> MyDate -> Bool
is_older d1 d2 
    | year d1 < year d2 = True
    | year d1 > year d2 = False
    | otherwise = if ( month d1 < month d2 )
                      then True
                  else if (month d1 == month d2)
                      then 
                          if (day d1 < day d2)
                          then True
                          else False
                  else False 

-- 2) take a list of dates and a month (i.e., an int) and returns
-- how many dates in the list are in the given month
number_in_month :: [MyDate] -> Integer -> Int
number_in_month dates m = length $ filter (\x-> x == m) $ map (\d -> month d) dates

-- 3) take a list of dates and a list of months (i.e., an int list)
-- and return the number of dates in the list of dates
-- that are in any of the months in the list of months.
number_in_months :: [MyDate] -> [Integer] -> [Int]
number_in_months dates months = map (\x -> number_in_month dates x) (rmdups months)

-- 4) take a list of dates and a month (i.e., an int) and return a
-- list holding the dates from the argument list of dates that are in the month.
-- The returned list should contain dates in the order they were originally given.
dates_in_month :: [MyDate] -> Integer -> [MyDate]
dates_in_month dates m = filter (\x -> month x == m) dates

-- 5) take a list of dates and a list of month (i.e., an int list)
-- and returns a list holding the dates from the argument list
-- of dates that are in any of the months in the list of months.
dates_in_months :: [MyDate] -> [Integer] -> [[MyDate]]
dates_in_months dates months = map (\x -> dates_in_month dates x) (rmdups months)

-- 6) take a list of strings and an int n and return the n th element of the
-- list where the head of the list is 1 st
get_nth :: [String] -> Integer -> Maybe String
-- get_nth = xs !! n
get_nth [] idx = Nothing
get_nth xs 1 = Just $ head xs
get_nth (x:xs) idx = get_nth xs (idx-1)

-- 7) take an int called sum, which you can assume
-- is positive, and an int list, which you can assume contains
-- all positive numbers, and returns an int.
-- You should return an int n such that the first n elements of the list
-- add to less than sum, but the first n + 1 elements of the list add to sum or more.
-- Assume the entire list sums to more than the passed in value;
-- it is okay for an exception to occur if this is not the case. 
number_before_reaching_sum :: Integer -> [Integer] -> Integer
number_before_reaching_sum sum [] = 0
number_before_reaching_sum sum xs
    | sum <= (head xs) = 0
    | otherwise = 1 + number_before_reaching_sum (sum - head xs) (tail xs)

-- 8) take a day of year (i.e., an int between 1 and 365)
-- and return what month that day is in
what_month :: Integer -> Integer
what_month day =
    1 + number_before_reaching_sum day monthdays where
        monthdays = [31,28,31,30,31,30,31,31,30,31,30,31]

-- 9) takes two days of the year day1 and day2 and returns an int list
-- [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of
-- day1+1, ..., and mn is the month of day day2.
month_range :: Integer -> Integer -> [Integer]
month_range day1 day2 = map what_month [day1..day2]

-- 10) find an oldest date from a list
oldest :: [MyDate] -> Maybe MyDate
oldest [] = Nothing
oldest (x:[]) = Just x
oldest (x:xs) =
    case oldest_rest of
        Just a -> Just o where
            o | (is_older x a) = x | otherwise = a
        Nothing ->  Just x
    where oldest_rest = oldest xs


-- remove duplicates
-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell
-- import qualified Data.Set as Set
rmdups :: Ord a => [a] -> [a]
rmdups = S.toList . S.fromList 

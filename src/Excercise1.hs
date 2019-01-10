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
      MyDate(..) -- to export constructor & methods as well 
    ) where

type D = MyDate
data MyDate = D
    { 
      year :: Integer,
      month :: Integer,
      day :: Integer
    }

instance Show MyDate where
    show d = show (day d) ++ "-" ++
        show (month d) ++ "-" ++
        show (year d)

instance Eq MyDate where
    x == y = day x == day y &&
             month x == month y &&
             year x == year y


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
-- Assume the list of months has no number repeated.
number_in_months :: [MyDate] -> [Integer] -> [Int]
number_in_months dates months = map (\x -> number_in_month dates x) months

-- 4) take a list of dates and a month (i.e., an int) and return a
-- list holding the dates from the argument list of dates that are in the month.
-- The returned list should contain dates in the order they were originally given.
dates_in_month :: [MyDate] -> Integer -> [MyDate]
dates_in_month dates m = filter (\x -> month x == m) dates

-- 5) take a list of dates and a list of month (i.e., an int list)
-- and returns a list holding the dates from the argument list
-- of dates that are in any of the months in the list of months.
-- Assume the list of months has no number repeated.
dates_in_months :: [MyDate] -> [Integer] -> [[MyDate]]
dates_in_months dates months = map (\x -> dates_in_month dates x) months

-- 6) take a list of strings and an int n and return the n th element of the
-- list where the head of the list is 1 st
get_nth :: [String] -> Int -> Maybe String
-- get_nth = xs !! n
get_nth [] idx = Nothing
get_nth xs 1 = Just $ head xs
get_nth (x:xs) idx = get_nth xs (idx-1)

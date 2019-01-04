-- Write 11 functions to compare dates
-- Date is represented with three integers
module Excercise1
    ( someFunc,
      is_older,
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

someFunc :: IO ()
someFunc = print $ day $D 2018 12 24

-- check if the first date is older
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

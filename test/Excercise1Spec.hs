module Excercise1Spec (spec) where

import Control.Arrow
import Test.Hspec

import Excercise1


spec = do
  describe "Excercise1 library tests" pExcercise1LibSpec

pExcercise1LibSpec = do
  let d1 = D 2018 12 24
  let d2 = D 2018 12 24
  let d3 = D 2018 01 24
  let d4 = D 2018 12 23
  let d5 = D 2017 12 24
  it "for is_older"  $ do
    let uc_is_older = uncurry is_older
    let correct = map ( uc_is_older *** \x -> x)
                    [((d1, d2), False),
                     ((d1, d3), False),
                     ((d1, d4), False),
                     ((d5, d1), True),
                     ((d4, d1), True),
                     ((d3, d2), True)]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
  it "for number_in_month"  $ do
    let uc_number_in_month = uncurry number_in_month
    let correct = map ( uc_number_in_month *** \x -> x)
                    [(([d1, d2, d3, d4, d5], 12), 4),
                     (([d1, d2], 12), 2),
                     (([d1, d2, d3], 1), 1),
                     (([], 1), 0),
                     (([d1, d2, d3, d4, d5], 3), 0)]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
  it "for number_in_months"  $ do
    let uc_number_in_months = uncurry number_in_months
    let correct = map ( uc_number_in_months *** \x -> x)
                    [(([d1, d2, d3, d4, d5], [12, 1]), [4, 1]),
                     (([d1, d2], [12, 1]), [2, 0]),
                     (([d1, d2, d3], [1, 12, 3]), [1, 2, 0]),
                     (([], [1]), [0]),
                     (([], [1, 2, 3]), [0, 0, 0]),
                     (([d1, d2, d3, d4, d5], [3, 4]), [0, 0])]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
  it "for dates_in_month"  $ do
    let uc_dates_in_month = uncurry dates_in_month
    let correct = map ( uc_dates_in_month *** \x -> x)
                    [(([d1, d2, d3, d4, d5], 12), [d1, d2, d4, d5]),
                     (([d1, d2, d3, d4, d5], 1), [d3]),
                     (([d1, d2, d3, d4, d5], 4), []),
                     (([d1, d2], 1), []),
                     (([], 1), [])]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
  it "for dates_in_months"  $ do
    let uc_dates_in_months = uncurry dates_in_months
    let correct = map ( uc_dates_in_months *** \x -> x)
                    [(([d1, d2, d3, d4, d5], [12, 1]), [[d1, d2, d4, d5], [d3]]),
                     (([d1, d2], [12, 1]), [[d1, d2], []]),
                     (([d1, d2, d3], [1, 12, 3]), [[d3], [d1, d2], []]),
                     (([], [1]), [[]]),
                     (([], [1, 2, 3]), [[], [], []]),
                     (([d1, d2, d3, d4, d5], [3, 4]), [[], []])]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
  it "for get_nth"  $ do
    let uc_get_nth = uncurry get_nth
    let xs = ["a", "b", "c", "dd", "eee", "ffff"]
    let correct = map ( uc_get_nth *** \x -> x)
                    [((xs, 1), Just "a" ),
                     ((xs, 3), Just "c"),
                     ((xs, 4), Just "dd"),
                     ((xs, 5), Just "eee"),
                     ((xs, 6), Just "ffff"),
                     (([], 1), Nothing)]
    mapM_ (`shouldSatisfy` uncurry (==)) correct
    

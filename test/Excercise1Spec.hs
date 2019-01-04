module Excercise1Spec (spec) where

import Control.Arrow
import Test.Hspec

import Excercise1


spec = do
  describe "Excercise1 library tests" pExcercise1LibSpec

pExcercise1LibSpec = do
  it "for is_older"  $ do
    let d1 = D 2018 12 24
    let d2 = D 2018 12 24
    let d3 = D 2018 01 24
    let d4 = D 2018 12 23
    let d5 = D 2017 12 24
    let uc_is_older = uncurry is_older
    let correct = map ( uc_is_older *** \x -> x)
                    [((d1, d2), False),
                     ((d1, d3), False),
                     ((d1, d4), False),
                     ((d5, d1), True),
                     ((d4, d1), True),
                     ((d3, d2), True)]
    mapM_ (`shouldSatisfy` uncurry (==)) correct

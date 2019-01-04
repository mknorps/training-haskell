module Main where

import qualified Excercise1Spec as ES1

import Test.Hspec

main = hspec $ do
  ES1.spec

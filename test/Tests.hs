
module Main
  (
    main
  ) where

import Test.Tasty
import qualified SimpleStats.Tests.Basic as Basic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "SimpleStats"
  [
    Basic.tests
  ]

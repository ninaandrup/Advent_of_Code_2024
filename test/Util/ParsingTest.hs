module Util.ParsingTest (parsingTests) where

import Test.HUnit
import Util.Parsing (splitStringAt)

-- Test cases
testSplitStringAt :: Test
testSplitStringAt =
  TestList
    [ "split at ',' (none)" ~: splitStringAt ',' "hello world" ~?= ("hello world", ""),
      "split at ',' (one)" ~: splitStringAt ',' "hello, world" ~?= ("hello", " world"),
      "split at ',' (multiple)" ~: splitStringAt ',' "hello, hello, world" ~?= ("hello", " hello, world")
    ]

-- Group all tests
parsingTests :: Test
parsingTests = TestList [testSplitStringAt]

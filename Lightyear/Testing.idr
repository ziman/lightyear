-- ------------------------------------------------------------- [ Char.idr ]
-- Module      : Lightyear.Testing
-- Description : Testing utilities.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
--
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Testing

import Lightyear
import Lightyear.Strings

%access  export
%default total

||| Simple data structure to store the result of a parsing test.
data TestReport : Type where
  Pass : (title : String) -> TestReport

  ParseFailure : (title : String)
              -> (report : String)
              -> TestReport

  ResultFailure : (title : String) -> (made : String) -> TestReport

  ||| Report a parsing failure with expected failure
  AdvParseFailure : Show a
                 => (title    : String)
                 -> (input    : String)
                 -> (expected : a)
                 -> (report   : String)
                 -> TestReport

  ||| Report a wrong result failure with expected failure.
  AdvResultFailure : Show a
                 => (title    : String)
                 -> (input    : String)
                 -> (expected : a)
                 -> (actual   : a)
                 -> TestReport



||| Run a parsing test that is expected to pass and discard result.
|||
||| @title    Name of the test.
||| @p        The parser to test.
||| @input    The input string to parse.
parseTest : Show a
         => (title    : String)
         -> (p        : Parser a)
         -> (input    : String)
         -> TestReport
parseTest title p input = do
  case parse p input of
    Left err     => ParseFailure title err
    Right actual => Pass title

||| Run a parsing test that is expected to fail and discard result.
|||
||| @title    Name of the test.
||| @p        The parser to test.
||| @input    The input string to parse.
parseTestNot : Show a
            => (title    : String)
            -> (p        : Parser a)
            -> (input    : String)
            -> TestReport
parseTestNot title p input = do
  case parse p input of
    Left err     => Pass title
    Right actual => ResultFailure title (show actual)


||| Run a parsing test that is expected to pass and compare result to
||| an expected value.
|||
||| @title    Name of the test.
||| @p        The parser to test.
||| @input    The input string to parse.
||| @expected The expected output from a successful parsing.
||| @eq       A comparison function.
parseTestCmp : Show a
            => (title    : String)
            -> (p        : Parser a)
            -> (eq       : a -> a -> Bool)
            -> (input    : String)
            -> (expected : a)
            -> TestReport
parseTestCmp title p test input expected = do
  case parse p input of
    Left err     => AdvParseFailure title input expected err
    Right actual => if test actual expected
                      then Pass title
                      else AdvResultFailure title input expected actual

||| Run a parsing test that is expected to fail.
|||
||| @title    Name of the test.
||| @p        The parser to test.
||| @input    The input string to parse.
||| @expected The expected parsing error message.
parseTestCmpNot : Show a
               => (title    : String)
               -> (p        : Parser a)
               -> (input    : String)
               -> (expected : String)
               -> TestReport
parseTestCmpNot title p input expected = do
  case parse p input of
    Left actual  => if trim actual == trim expected
                      then Pass title
                      else AdvParseFailure title input expected actual
    Right result => AdvResultFailure title input result result

||| Run a parsing test that is expected to pass and compare showing
||| the result to an expected value.
|||
||| @title    Name of the test.
||| @p        The parser to test.
||| @input    The input string to parse.
||| @expected The expected output from a successful parsing.
parseTestCmpShow : Show a
                => (title    : String)
                -> (p        : Parser a)
                -> (input    : String)
                -> (expected : String)
                -> TestReport
parseTestCmpShow title p input expected = do
  case parse p input of
    Left err     => ParseFailure title  err
    Right actual => if show actual == expected
                      then Pass title
                      else ResultFailure title (show actual)

||| Turn the test report into a string.
private
showReport : TestReport -> String
showReport (Pass title) = unwords ["[PASS]", title]

showReport (ParseFailure title report) =
  unlines [ unwords ["[FAIL]", "[PARSING]", title]
          , unwords ["\t", "Gave error:"]
          , report
          ]
showReport (ResultFailure title made) =
  unlines [ unwords ["[FAIL]", "[RESULT]", title]
            , unwords ["\t", "I was able to parse, when I shouldn't and made:"]
            , unwords ["\t\t", made]
           ]

showReport (AdvParseFailure title input expected report) =
  unlines [ unwords ["[FAIL]", "[PARSING]", title]
          , unwords ["\t", "Given input:"]
          , unwords ["\t\t", input]
          , unwords ["\t", "Gave error:"]
          , report
          ]
showReport (AdvResultFailure title input expected actual) =
    unlines $ head ++ footer
  where
    head = [ unwords ["[FAIL]", "[RESULT]", title]
            , unwords ["\t", "Given input:"]
            , unwords ["\t\t", input]
           ]
    footer = [ unwords ["\t", "I made:"]
             , unwords ["\t\t", show actual]
             , unwords ["\t", "But was expected to make:"]
             , unwords ["\t\t", show expected]
             ]

||| Run an individual test.
runTest : TestReport -> IO ()
runTest = (putStrLn . showReport)

||| Run a sequence of tests.
runTests : List TestReport -> IO ()
runTests = traverse_ runTest

-- --------------------------------------------------------------------- [ EOF ]

module Test

import Data.Vect
import Data.Fin

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Testing

%access export

listOf : Parser a -> Parser (List a)
listOf p = string "[" *!> (p `sepBy` string ",") <* string "]"

listOf' : Parser a -> Parser (List a)
listOf' p = brackets (commaSep p)

nat : Parser Nat
nat = integer

tests : IO ()
tests = runTests
  [ parseTestCmp "Test 1" nat          (==) "123"        123
  , parseTestCmp "Test 2" (listOf nat) (==) "[1,2,3,99]" [1,2,3,99]

  , parseTestCmpNot "Test 3" (listOf nat) "foo" """at 1:1 expected:
  string "["
at 1:1 expected:
  character '['
at 1:1 expected:
  a different token
"""

  -- should commit and fail
  , parseTestCmpNot "Test 4" (listOf nat <|> (string "[foo" *> pure List.Nil)) "[foo" """at 1:2 expected:
  string "]"
at 1:2 expected:
  character ']'
at 1:2 expected:
  a different token
"""

  , parseTestCmp "Test 5" (listOf $ listOf nat)   (==) "[[1,2],[],[3,4,5]]" [[1, 2], [], [3, 4, 5]]
  , parseTestCmp "Test 6" (listOf' nat)           (==) "[1,2,3,99]"         [1, 2, 3, 99]
  , parseTestCmp "Test 6" (listOf' $ listOf' nat) (==) "[[1,2],[],[3,4,5]]" [[1, 2], [], [3, 4, 5]]

  -- should commit and fail
  , parseTestCmp "Test 7" (listOf' nat <|> (string "[foo" *> pure List.Nil)) (==) "[foo" []
  ]

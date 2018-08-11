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

  , parseTestCmpNot "Test 3" (listOf nat) "foo" "At 1:1:\n\tstring \"[\"\nAt 1:1:\n\tcharacter '['\nAt 1:1:\n\ta different token"

  -- should commit and fail
  , parseTestCmpNot "Test 4" (listOf nat <|> (string "[foo" *> pure List.Nil)) "[foo" "At 1:2:\n\tstring \"]\"\nAt 1:2:\n\tcharacter ']'\nAt 1:2:\n\ta different token"

  , parseTestCmp "Test 5" (listOf $ listOf nat)   (==) "[[1,2],[],[3,4,5]]" [[1, 2], [], [3, 4, 5]]
  , parseTestCmp "Test 6" (listOf' nat)           (==) "[1,2,3,99]"         [1, 2, 3, 99]
  , parseTestCmp "Test 6" (listOf' $ listOf' nat) (==) "[[1,2],[],[3,4,5]]" [[1, 2], [], [3, 4, 5]]

  -- should commit and fail
  , parseTestCmp "Test 7" (listOf' nat <|> (string "[foo" *> pure List.Nil)) (==) "[foo" []

  -- should NOT commit
  , parseTestCmp "Test 8" ((getPosition *> string "x") <|> string "y") (==) "y" "y"

  -- should not eat up the one "b"
  , parseTestCmp "Test 9" (ntimes 4 (requireFailure (string "bb") *> anyChar)) (==) "abcde" ['a','b','c','d']
  ]

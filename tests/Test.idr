module Main

import Data.Vect
import Data.Fin

import Lightyear
import Lightyear.Strings

test : Show a => Parser a -> String -> IO ()
test p input = case parse p input of
  Left  e => putStrLn e
  Right x => printLn x

listOf : Parser a -> Parser (List a)
listOf p = string "[" *!> (p `sepBy` string ",") <* string "]"

listOf' : Parser a -> Parser (List a)
listOf' p = brackets (commaSep p)

nat : Parser Nat
nat = integer

main : IO ()
main = do
  test nat "123"
  test (listOf nat) "[1,2,3,99]"
  test (listOf nat) "foo"
  test (listOf nat <|> (string "[foo" *> pure List.Nil)) "[foo"  -- should commit and fail
  test (listOf $ listOf nat) "[[1,2],[],[3,4,5]]"

  test (listOf' nat) "[1,2,3,99]"
  test (listOf' $ listOf' nat) "[[1,2],[],[3,4,5]]"
  test (listOf' nat <|> (string "[foo" *> pure List.Nil)) "[foo"  -- should commit and fail

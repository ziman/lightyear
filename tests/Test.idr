module Main

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings

test : Show a => Parser a -> String -> IO ()
test p input = case parse p input of
  Left  e => putStrLn e
  Right x => print x

main : IO ()
main = do
  test (the (Parser Nat) integer) "123"

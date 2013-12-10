module Lightyear.Strings

-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Errmsg

%access public

private
nat2int : Nat -> Int
nat2int  Z    = 0
nat2int (S x) = 1 + nat2int x

instance Layout String where
  lineLengths = map (nat2int . Prelude.Strings.length) . lines

Parser : Type -> Type
Parser = ParserT Identity String

parse : Parser a -> String -> Either String a
parse (PT f) s = let Id r = f s in case r of
  Success _ x  => Right x
  Failure _ es => Left $ formatError s es

private
uncons : String -> Maybe (Char, String)
uncons s with (strM s)
  uncons ""             | StrNil       = Nothing
  uncons (strCons x xs) | StrCons x xs = Just (x, xs)

private
c2s : Char -> String
c2s c = pack (c :: Prelude.List.Nil)

satisfy : Monad m => (Char -> Bool) -> ParserT m String Char
satisfy = satisfy' (St uncons)

char : Monad m => Char -> ParserT m String ()
char c = skip (satisfy (== c)) <?> "character '" ++ c2s c ++ "'"

string : Monad m => String -> ParserT m String ()
string s = traverse_ char (unpack s) <?> "string " ++ show s

space : Monad m => ParserT m String ()
space = skip (many $ satisfy isSpace) <?> "whitespace"

token : Monad m => String -> ParserT m String ()
token s = skip (string s) <$ space <?> "token " ++ show s

parens : Monad m => ParserT m String a -> ParserT m String a
parens p = char '(' $> p <$ char ')'

test : Parser a -> String -> IO (Maybe a)
test p s = case parse p s of
  Left  e => putStrLn e $> pure Nothing
  Right x => pure (Just x)

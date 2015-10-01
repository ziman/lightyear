-- ------------------------------------------------------------- [ Strings.idr ]
-- Module      : Lightyear.Strings
-- Description : String-related parsers.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Strings

import public Data.Vect
import public Data.Fin

import public Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Errmsg

import Lightyear.Char

%access public

-- -------------------------------------------------------- [ Helper Functions ]
private
nat2int : Nat -> Int
nat2int  Z    = 0
nat2int (S x) = 1 + nat2int x

instance Layout String where
  lineLengths = map (nat2int . Prelude.Strings.length) . lines

-- --------------------------------------------------------- [ A String Parser ]
||| Parsers, specialised to Strings
Parser : Type -> Type
Parser = ParserT Identity String

||| Run a parser against an input string
parse : Parser a -> String -> Either String a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es  => Left $ formatError s es

instance Stream Char String where
  uncons s with (strM s)
    uncons ""             | StrNil       = Nothing
    uncons (strCons x xs) | StrCons x xs = Just (x, xs)

-- ---------------------------------------------------------- [ Reserved Stuff ]

||| A parser that matches a particular string
string : Monad m => String -> ParserT m String String
string s = pack <$> (traverse char $ unpack s) <?> "string " ++ show s

-- ------------------------------------------------------------------- [ Space ]

||| A simple lexer that strips white space from tokens
lexeme : Monad m => ParserT m String a -> ParserT m String a
lexeme p = p <* spaces

-- ------------------------------------------------------------------ [ Tokens ]

||| A parser that matches a specific string, then skips following whitespace
token : Monad m => String -> ParserT m String ()
token s = lexeme (skip (string s)) <?> "token " ++ show s

||| Parses ',' and trailing whitespace.
comma : Monad m => ParserT m String ()
comma = token "," <?> "Comma"

||| Parses '=' and trailing whitespace.
equals : Monad m => ParserT m String ()
equals = token "=" <?> "equals"

||| Parses '.' and trailing whitespace.
dot : Monad m => ParserT m String ()
dot = token "." <?> "dot"

||| Parses ':' and trailing whitespace.
colon : Monad m => ParserT m String ()
colon = token ":" <?> "colon"

||| Parses ';' and trailing whitespace.
semi : Monad m => ParserT m String ()
semi = token ";" <?> "semi colon"

-- -------------------------------------------------- [ Delineated Expressions ]

||| Parses `p` enclosed in parenthesis and returns result of `p`.
parens : Monad m => ParserT m String a -> ParserT m String a
parens p = between (token "(") (token ")") p

||| Parses `p` enclosed in brackets and returns result of `p`.
brackets : Monad m => ParserT m String a -> ParserT m String a
brackets p = between (token "[") (token "]") p

||| Parses `p` enclosed in braces and returns the result of `p`.
braces : Monad m => ParserT m String a -> ParserT m String a
braces p = between (token "{") (token "}") p

||| Parses `p` enclosed in angles and returns the result of `p`.
angles : Monad m => ParserT m String a -> ParserT m String a
angles p = between (token "<") (token ">") p

||| Parses `p` enclosed in single quotes and returns the result of `p`.
||| Not to be used for charLiterals.
squote : Monad m => ParserT m String a -> ParserT m String a
squote p = between (char '\'') (char '\'') p

||| Parses `p` enclosed in double quotes and returns the result of `p`.
||| Not to be used for `stringLiterals`.
dquote : Monad m => ParserT m String a -> ParserT m String a
dquote p = between (char '\"') (char '\"') p

||| Collect the literal string contained between two characters
quoted' : Monad m => Char -> Char -> ParserT m String String
quoted' l r = map pack $ between (char l) (char r) (some (satisfy (/= r)))

||| Literal string between two identical characters
quoted : Monad m => Char -> ParserT m String String
quoted c = quoted' c c

-- --------------------------------------------------- [ Separated Expressions ]
||| Parses /one/ or more occurrences of `p` separated by `comma`.
commaSep1 : Monad m => ParserT m String a -> ParserT m String (List a)
commaSep1 p = p `sepBy1` comma

||| Parses /zero/ or more occurrences of `p` separated by `comma`.
commaSep : Monad m => ParserT m String a -> ParserT m String (List a)
commaSep p = p `sepBy` comma

||| Parses /one/ or more occurrences of `p` separated by `semi`.
semiSep1 : Monad m => ParserT m String a -> ParserT m String (List a)
semiSep1 p = p `sepBy1` semi

||| Parses /zero/ or more occurrences of `p` separated by `semi`.
semiSep : Monad m => ParserT m String a -> ParserT m String (List a)
semiSep p = p `sepBy` semi

-- -------------------------------------------------------- [ Testing Function ]

testParser : Parser a -> String -> IO (Maybe a)
testParser p s = case parse p s of
  Left  e => putStrLn e *> pure Nothing
  Right x => pure (Just x)
-- --------------------------------------------------------------------- [ EOF ]

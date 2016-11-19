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

%access export

-- -------------------------------------------------------- [ Helper Functions ]
private
nat2int : Nat -> Int
nat2int  Z    = 0
nat2int (S x) = 1 + nat2int x

implementation Layout String where
  lineLengths = map (nat2int . Prelude.Strings.length) . lines

-- --------------------------------------------------------- [ A String Parser ]
||| Parsers, specialised to Strings
public export
Parser : Type -> Type
Parser = ParserT String Identity

||| Run a parser against an input string
parse : Parser a -> String -> Either String a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es  => Left $ formatError s es

implementation Stream Char String where
  uncons s with (strM s)
    uncons ""             | StrNil       = Nothing
    uncons (strCons x xs) | StrCons x xs = Just (x, xs)

-- ---------------------------------------------------------- [ Reserved Stuff ]

||| A parser that matches a particular string
string : Monad m => String -> ParserT String m String
string s = pack <$> (traverse char $ unpack s) <?> "string " ++ show s

-- ------------------------------------------------------------------- [ Space ]

||| A simple lexer that strips white space from tokens
lexeme : Monad m => ParserT String m a -> ParserT String m a
lexeme p = p <* spaces

-- ------------------------------------------------------------------ [ Tokens ]

||| A parser that matches a specific string, then skips following whitespace
token : Monad m => String -> ParserT String m ()
token s = lexeme (skip (string s)) <?> "token " ++ show s

||| Parses ',' and trailing whitespace.
comma : Monad m => ParserT String m ()
comma = token "," <?> "Comma"

||| Parses '=' and trailing whitespace.
equals : Monad m => ParserT String m ()
equals = token "=" <?> "equals"

||| Parses '.' and trailing whitespace.
dot : Monad m => ParserT String m ()
dot = token "." <?> "dot"

||| Parses ':' and trailing whitespace.
colon : Monad m => ParserT String m ()
colon = token ":" <?> "colon"

||| Parses ';' and trailing whitespace.
semi : Monad m => ParserT String m ()
semi = token ";" <?> "semi colon"

-- -------------------------------------------------- [ Delineated Expressions ]

||| Parses `p` enclosed in parenthesis and returns result of `p`.
parens : Monad m => ParserT String m a -> ParserT String m a
parens p = between (token "(") (token ")") p

||| Parses `p` enclosed in brackets and returns result of `p`.
brackets : Monad m => ParserT String m a -> ParserT String m a
brackets p = between (token "[") (token "]") p

||| Parses `p` enclosed in braces and returns the result of `p`.
braces : Monad m => ParserT String m a -> ParserT String m a
braces p = between (token "{") (token "}") p

||| Parses `p` enclosed in angles and returns the result of `p`.
angles : Monad m => ParserT String m a -> ParserT String m a
angles p = between (token "<") (token ">") p

||| Parses `p` enclosed in single quotes and returns the result of `p`.
||| Not to be used for charLiterals.
squote : Monad m => ParserT String m a -> ParserT String m a
squote p = between (char '\'') (lexeme $ char '\'') p

||| Parses `p` enclosed in double quotes and returns the result of `p`.
||| Not to be used for `stringLiterals`.
dquote : Monad m => ParserT String m a -> ParserT String m a
dquote p = between (char '\"') (lexeme $ char '\"') p

||| Collect the literal string contained between two characters
quoted' : Monad m => Char -> Char -> ParserT String m String
quoted' l r = map pack $ between (char l) (lexeme $ char r) (some (satisfy (/= r)))

||| Literal string between two identical characters
quoted : Monad m => Char -> ParserT String m String
quoted c = quoted' c c

-- --------------------------------------------------- [ Separated Expressions ]
||| Parses /one/ or more occurrences of `p` separated by `comma`.
commaSep1 : Monad m => ParserT String m a -> ParserT String m (List a)
commaSep1 p = p `sepBy1` comma

||| Parses /zero/ or more occurrences of `p` separated by `comma`.
commaSep : Monad m => ParserT String m a -> ParserT String m (List a)
commaSep p = p `sepBy` comma

||| Parses /one/ or more occurrences of `p` separated by `semi`.
semiSep1 : Monad m => ParserT String m a -> ParserT String m (List a)
semiSep1 p = p `sepBy1` semi

||| Parses /zero/ or more occurrences of `p` separated by `semi`.
semiSep : Monad m => ParserT String m a -> ParserT String m (List a)
semiSep p = p `sepBy` semi

||| Run some parser `p` until the second parser is encountered,
||| collecting a list of success for `p`, and the result of the second
||| parser is dropped.
|||
||| Primarily useful for collecting single line comments and other
||| similar verbatim environments.
manyTill : Monad m => ParserT String m a
                   -> ParserT String m b
                   -> ParserT String m (List a)
manyTill p end = scan
  where
    scan : Monad m => ParserT String m (List a)
    scan = do { end; pure List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}


-- -------------------------------------------------------- [ Testing Function ]

testParser : Parser a -> String -> IO (Maybe a)
testParser p s = case parse p s of
  Left  e => putStrLn e *> pure Nothing
  Right x => pure (Just x)
-- --------------------------------------------------------------------- [ EOF ]

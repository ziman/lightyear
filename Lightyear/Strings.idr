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

||| Parsers, specialised to Strings
Parser : Type -> Type
Parser = ParserT Identity String

||| Run a parser against an input string
parse : Parser a -> String -> Either String a
parse f s = let Id r = execParserT f s in case r of
  Success _ x => Right x
  Failure es  => Left $ formatError s es

private
uncons : String -> Maybe (Char, String)
uncons s with (strM s)
  uncons ""             | StrNil       = Nothing
  uncons (strCons x xs) | StrCons x xs = Just (x, xs)

||| Matches a single character that satisfies some condition
satisfy : Monad m => (Char -> Bool) -> ParserT m String Char
satisfy = satisfy' (St uncons)

||| Matches a single character that satsifies some condition, accepting a transformation of successes
satisfyMaybe : Monad m => (Char -> Maybe out) -> ParserT m String out
satisfyMaybe = satisfyMaybe' (St uncons)

||| A parser that matches some particular character
char : Monad m => Char -> ParserT m String ()
char c = skip (satisfy (== c)) <?> "character '" ++ singleton c ++ "'"

||| A parser that matches a particular string
string : Monad m => String -> ParserT m String ()
string s = traverse_ char (unpack s) <?> "string " ++ show s

||| A parser that skips whitespace
space : Monad m => ParserT m String ()
space = skip (many $ satisfy isSpace) <?> "whitespace"

||| A simple lexer that strips white space from tokens
lexeme : Monad m => ParserT m String a -> ParserT m String a
lexeme p = p <$ space

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

||| Matches a single digit
digit : Monad m => ParserT m String (Fin 10)
digit = satisfyMaybe fromChar
  where fromChar : Char -> Maybe (Fin 10)
        fromChar '0' = Just FZ
        fromChar '1' = Just (FS (FZ))
        fromChar '2' = Just (FS (FS (FZ)))
        fromChar '3' = Just (FS (FS (FS (FZ))))
        fromChar '4' = Just (FS (FS (FS (FS (FZ)))))
        fromChar '5' = Just (FS (FS (FS (FS (FS (FZ))))))
        fromChar '6' = Just (FS (FS (FS (FS (FS (FS (FZ)))))))
        fromChar '7' = Just (FS (FS (FS (FS (FS (FS (FS (FZ))))))))
        fromChar '8' = Just (FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))))
        fromChar '9' = Just (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))))
        fromChar _   = Nothing

||| Matches an integer literal
integer : (Num n, Monad m) => ParserT m String n
integer = do minus <- opt (char '-')
             ds <- some digit
             let theInt = getInteger ds
             case minus of
               Nothing => pure (fromInteger theInt)
               Just () => pure (fromInteger ((-1) * theInt))
  where getInteger : List (Fin 10) -> Integer
        getInteger = foldl (\a => \b => 10 * a + cast b) 0


testParser : Parser a -> String -> IO (Maybe a)
testParser p s = case parse p s of
  Left  e => putStrLn e $> pure Nothing
  Right x => pure (Just x)

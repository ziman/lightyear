-- ------------------------------------------------------------- [ Char.idr ]
-- Module      : Lightyear.Char
-- Description : Character-related parsers.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
--
-- This code is (mostly) a port of Daan Leijen's Text.Parsec.Char library.
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Char

import public Data.Vect
import public Data.Fin

import public Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators

%access export

||| A parser that matches some particular character
char : (Monad m, Stream Char str) => Char -> ParserT str m Char
char c = satisfy (== c) <?> "character '" ++ singleton c ++ "'"

||| oneOf cs succeeds if the current character is in the supplied
||| list of characters @cs@. Returns the parsed character. See also
||| 'satisfy'.
|||
|||   vowel  = oneOf "aeiou"
oneOf :  (Monad m, Stream Char str) => String -> ParserT str m Char
oneOf cs = satisfy (\c => elem c $ unpack cs)

||| As the dual of 'oneOf', @noneOf cs@ succeeds if the current
||| character /not/ in the supplied list of characters @cs@. Returns the
||| parsed character.
|||
|||  consonant = noneOf "aeiou"
noneOf :  (Monad m, Stream Char str) => String -> ParserT str m Char
noneOf cs = satisfy (\c => not $ elem c $ unpack cs)

||| Parses a white space character (any character which satisfies 'isSpace')
||| Returns the parsed character.
space : (Monad m, Stream Char s) => ParserT s m Char
space               = satisfy isSpace       <?> "space"

||| Skips /zero/ or more white space characters. See also 'skipMany'.
spaces : (Monad m, Stream Char s) => ParserT s m ()
spaces              = skip (many Lightyear.Char.space)  <?> "white space"

||| Parses a newline character (\'\\n\'). Returns a newline character.
newline : (Monad m, Stream Char s) => ParserT s m Char
newline = char '\n' <?> "lf new-line"

||| Parses a carriage return character (\'\\r\') followed by a newline character (\'\\n\').
||| Returns a newline character.
crlf : (Monad m, Stream Char s) => ParserT s m Char
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

||| Parses a CRLF (see 'crlf') or LF (see 'newline') end-of-line.
||| Returns a newline character (\'\\n\').
|||
||| endOfLine = newline <|> crlf
endOfLine : (Monad m, Stream Char s) => ParserT s m Char
endOfLine           = newline <|> crlf       <?> "new-line"

||| Parses a tab character (\'\\t\'). Returns a tab character.
tab : (Monad m, Stream Char s) => ParserT s m Char
tab                 = char '\t'             <?> "tab"

||| Parses an upper case letter (a character between \'A\' and \'Z\').
||| Returns the parsed character.
upper : (Monad m, Stream Char s) => ParserT s m Char
upper               = satisfy isUpper       <?> "uppercase letter"

||| Parses a lower case character (a character between \'a\' and \'z\').
||| Returns the parsed character.
lower : (Monad m, Stream Char s) => ParserT s m Char
lower               = satisfy isLower       <?> "lowercase letter"

||| Parses a letter or digit (a character between \'0\' and \'9\').
||| Returns the parsed character.

alphaNum : (Monad m, Stream Char s) => ParserT s m Char
alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

||| Parses a letter (an upper case or lower case character). Returns the
||| parsed character.
letter : (Monad m, Stream Char s) => ParserT s m Char
letter              = satisfy isAlpha       <?> "letter"

||| Matches a single digit
digit : (Monad m, Stream Char s) => ParserT s m (Fin 10)
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
integer : (Num n, Monad m, Stream Char s) => ParserT s m n
integer = do minus <- opt (char '-')
             ds <- some digit
             let theInt = getInteger ds
             case minus of
               Nothing => pure (fromInteger theInt)
               Just _  => pure (fromInteger ((-1) * theInt))
  where getInteger : List (Fin 10) -> Integer
        getInteger = foldl (\a => \b => 10 * a + cast b) 0



||| Parses a hexadecimal digit (a digit or a letter between \'a\' and
||| \'f\' or \'A\' and \'F\'). Returns the parsed character.
hexDigit : (Monad m, Stream Char s) => ParserT s m Char
hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

||| Parses an octal digit (a character between \'0\' and \'7\'). Returns
||| the parsed character.
octDigit : (Monad m, Stream Char s) => ParserT s m Char
octDigit            = satisfy isOctDigit    <?> "octal digit"

||| This parser succeeds for any character. Returns the parsed character.
anyChar : (Monad m, Stream Char s) => ParserT s m Char
anyChar             = anyToken              <?> "any character"

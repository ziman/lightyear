-- --------------------------------------------------------- [ Combinators.idr ]
-- Module      : Lightyear.Combinators
-- Description : Generic Combinators
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Combinators

import Data.Vect

import Lightyear.Core

%access export

-- --------------------------------------------------------------- [ Operators ]
infixr 3 :::
private
(:::) : a -> List a -> List a
(:::) x xs = x :: xs

infixr 3 ::.
private
(::.) : a -> Vect n a -> Vect (S n) a
(::.) x xs = x :: xs

-- ---------------------------------------------------- [ Multiple Expressions ]

||| Run some parser as many times as possible, collecting a list of
||| successes.
many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = (pure (:::) <*> p <*>| many p) <|> pure List.Nil

||| Run some parser `p` until the second parser is encountered,
||| collecting a list of success for `p`, and the result of the second
||| parser is dropped.
|||
||| Primarily useful for collecting single line comments and other
||| similar verbatim environments.
manyTill : Monad m => ParserT m String a
                   -> ParserT m String b
                   -> ParserT m String (List a)
manyTill p end = scan
  where
    scan : Monad m => ParserT m String (List a)
    scan = do { end; return List.Nil } <|>
           do { x <- p; xs <- scan; return (x::xs)}


||| Run the specified parser precisely `n` times, returning a vector
||| of successes.
ntimes : Monad m => (n : Nat)
                 -> ParserT m str a
                 -> ParserT m str (Vect n a)
ntimes    Z  p = pure Vect.Nil
ntimes (S n) p = [| p ::. ntimes n p |]

||| Like `many`, but the parser must succeed at least once
some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p ::: many p |]

-- --------------------------------------------------- [ Separated Expressions ]

||| Parse repeated instances of at least one `p`, separated by `s`,
||| returning a list of successes.
|||
||| @ p the parser for items
||| @ s the parser for separators
sepBy1 : Monad m => (p : ParserT m str a)
                 -> (s : ParserT m str b)
                 -> ParserT m str (List a)
sepBy1 p s = [| p ::: many (s *> p) |]

||| Parse zero or more `p`s, separated by `s`s, returning a list of
||| successes.
|||
||| @ p the parser for items
||| @ s the parser for separators
sepBy : Monad m => (p : ParserT m str a)
                -> (s : ParserT m str b)
                -> ParserT m str (List a)
sepBy p s = (p `sepBy1` s) <|> pure List.Nil

||| Parse precisely `n` `p`s, separated by `s`s, returning a vect of
||| successes.
|||
||| @ n how many to parse
||| @ p the parser for items
||| @ s the parser for separators
sepByN : Monad m => (n : Nat)
                 -> (p : ParserT m str a)
                 -> (s : ParserT m str b)
                 -> ParserT m str (Vect n a)
sepByN    Z  p s = pure Vect.Nil
sepByN (S n) p s = [| p ::. ntimes n (s *> p) |]

||| Alternate between matches of `p` and `s`, starting with `p`,
||| returning a list of successes from both.
alternating : Monad m => (p : ParserT m str a)
                      -> (s : ParserT m str a)
                      -> ParserT m str (List a)
alternating p s = (pure (:::) <*> p <*>| alternating s p) <|> pure List.Nil

||| Throw away the result from a parser
skip : Monad m => ParserT m str a -> ParserT m str ()
skip = map (const ())

||| Attempt to parse `p`. If it succeeds, then return the value. If it
||| fails, continue parsing.
opt : Monad m => (p : ParserT m str a) -> ParserT m str (Maybe a)
opt p = map Just p <|> pure Nothing

||| Parse open, then p, then close. Returns the result of `p`.
|||
||| @open The opening parser.
||| @close The closing parser.
||| @p The parser for the middle part.
between : Monad m => (open : ParserT m str a)
                  -> (close : ParserT m str a)
                  -> (p : ParserT m str b)
                  -> ParserT m str b
between open close p = open *> p <* close

-- The following names are inspired by the cut operator from Prolog

-- ---------------------------------------------------- [ Monad-like Operators ]

infixr 5 >!=
||| Committing bind
(>!=) : Monad m => ParserT m str a
                -> (a -> ParserT m str b)
                -> ParserT m str b
x >!= f = x >>= commitTo . f

infixr 5 >!
||| Committing sequencing
(>!) : Monad m => ParserT m str a
               -> ParserT m str b
               -> ParserT m str b
x >! y = x >>= \_ => commitTo y

-- ---------------------------------------------- [ Applicative-like Operators ]

infixl 2 <*!>
||| Committing application
(<*!>) : Monad m => ParserT m str (a -> b)
                 -> ParserT m str a
                 -> ParserT m str b
f <*!> x = f <*> commitTo x

infixl 2 <*!
(<*!) : Monad m => ParserT m str a
                -> ParserT m str b
                -> ParserT m str a
x <*! y = x <* commitTo y

infixl 2 *!>
(*!>) : Monad m => ParserT m str a
                -> ParserT m str b
                -> ParserT m str b
x *!> y = x *> commitTo y

-- ---------------------------------------------------------- [ Lazy Operators ]

infixl 2 <*|
(<*|) : Monad m => ParserT m str a
                -> Lazy (ParserT m str b)
                -> ParserT m str a
x <*| y = pure const <*> x <*>| y

infixl 2 *>|
(*>|) : Monad m => ParserT m str a
                -> Lazy (ParserT m str b)
                -> ParserT m str b
x *>| y = pure (const id) <*> x <*>| y
-- ---------------------------------------------------------------------- [ EF ]

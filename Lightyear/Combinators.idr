module Lightyear.Combinators

-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.

import Lightyear.Core

%access public

infixr 3 :::
private
(:::) : a -> List a -> List a
(:::) x xs = x :: xs

infixr 3 ::.
private
(::.) : a -> Vect n a -> Vect (S n) a
(::.) x xs = x :: xs

many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = [| p ::: lazy (many p) |] <|> pure List.Nil

ntimes : Monad m => (n : Nat) -> ParserT m str a -> ParserT m str (Vect n a)
ntimes Z     p = pure Vect.Nil
ntimes (S n) p = [| p ::. ntimes n p |]

some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p ::: many p |]

sepBy1 : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy1 p s = [| p ::: many (s $> p) |]

sepBy : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy p s = (p `sepBy1` s) <|> pure List.Nil

sepByN : Monad m => (n : Nat) -> ParserT m str a -> ParserT m str b -> ParserT m str (Vect n a)
sepByN Z     p s = pure Vect.Nil
sepByN (S n) p s = [| p ::. ntimes n (s $> p) |]

alternating : Monad m => ParserT m str a -> ParserT m str a -> ParserT m str (List a)
alternating p s = [| p ::: lazy (alternating s p) |] <|> pure List.Nil

skip : Monad m => ParserT m str a -> ParserT m str ()
skip = map (const ())

opt : Monad m => ParserT m str a -> ParserT m str (Maybe a)
opt p = map Just p <|> pure Nothing

-- the following names are inspired by the cut operator from Prolog

-- Monad-like operators

infixr 5 >!=
(>!=) : Monad m => ParserT m str a -> (a -> ParserT m str b) -> ParserT m str b
x >!= f = x >>= commitTo . f

infixr 5 >!
(>!) : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str b
x >! y = x >>= \_ => commitTo y

-- Applicative-like operators

infixl 2 <$!>
(<$!>) : Monad m => ParserT m str (a -> b) -> ParserT m str a -> ParserT m str b
f <$!> x = f <$> commitTo x

infixl 2 <$!
(<$!) : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str a
x <$! y = x <$ commitTo y

infixl 2 $!>
($!>) : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str b
x $!> y = x $> commitTo y

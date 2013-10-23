module Lightyear.Combinators

import Lightyear.Core

%access public

many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = [| p :: lazy (many p) |] <|> pure []

some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p :: many p |]

sepBy1 : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy1 p s = [| p :: many (s $> p) |]

sepBy : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy p s = (p `sepBy1` s) <|> pure []

alternating : Monad m => ParserT m str a -> ParserT m str a -> ParserT m str (List a)
alternating p s = [| p :: lazy (alternating s p) |] <|> pure []

skip : ParserT m str a -> ParserT m str ()
skip = map (const ())

module Lightyear.Combinators

import Lightyear.Core

many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = [| p :: many p |] <|> pure [] <?+> "many"

some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p :: many p |] <?+> "some"

sepBy1 : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy1 p s = [| p :: many (s $> p) |]

sepBy : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy p s = (p `sepBy1` s) <|> pure []

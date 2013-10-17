module Lightyear.Combinators

import Lightyear.Core

%access public

many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = (pure (::) <$> p <$*> many p) <|*> pure [] <?+> "many"
-- note: we need to use the lazy <$*> because of the recursion

some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p :: many p |] <?+> "some"

sepBy1 : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy1 p s = [| p :: many (s $> p) |]

sepBy : Monad m => ParserT m str a -> ParserT m str b -> ParserT m str (List a)
sepBy p s = (p `sepBy1` s) <|> pure []

alternating : Monad m => ParserT m str a -> ParserT m str a -> ParserT m str (List a)
alternating p s = (pure (::) <$> p <$*> alternating s p) <|> pure []
-- lazy <$*> to handle recursion correctly

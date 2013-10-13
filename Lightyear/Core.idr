module Lightyear.Core

data Tag = Lib | User
data Result str a = Success str a | Failure (List (Tag, str, String))

instance Functor (Result str) where
  map f (Success s x) = Success s (f x)
  map f (Failure es ) = Failure es

record ParserT : (m : Type -> Type) -> (str : Type) -> (a : Type) -> Type where
  PT : (runParserT : str -> m (Result str a)) -> ParserT m str a

instance Monad m => Functor (ParserT m str) where
  map f (PT p) = PT (map (map f) . p)

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\s => pure (Success s x))
  (<$>) (PT f) (PT x) = PT $ \s => f s >>= \f' => case f' of
    Failure es   => pure (Failure es)
    Success s' g => x s' >>= pure . map g

instance Monad m => Monad (ParserT m str) where
  (>>=) (PT x) f = PT $ \s => x s >>= \r => case r of
    Failure es   => pure (Failure es)
    Success s' y => let PT f' = f y in f' s'

fail : str -> String -> Result str a
fail s msg = Failure ((Lib, s, msg) :: [])

instance Monad m => Alternative (ParserT m str) where
  empty = PT (\s => pure . fail s $ "non-empty alternative")
  (<|>) (PT x) (PT y) = PT $ \s => x s >>= \r => case r of
    Success s' y => pure (Success s' y)
    Failure es   => y s

infixl 0 <??>
(<??>) : Monad m => ParserT m str a -> (Tag, String) -> ParserT m str a
(PT f) <??> (t, msg) = PT $ \s => f s >>= \r => case r of
   Failure es  => pure $ Failure ((t, s, msg) :: es)
   Success s x => pure $ Success s x

infixl 0 <?>
(<?>) : Monad m => ParserT m str a -> String -> ParserT m str a
p <?> msg = p <??> (User, msg)

infixl 0 <?->
private
(<?->) : Monad m => ParserT m str a -> String -> ParserT m str a
p <?-> msg = p <??> (Lib, msg)

c2s : Char -> String
c2s c = pack (c :: [])

skip : Functor f => f a -> f ()
skip = map (const ())

satisfy : Monad m => (Char -> Bool) -> ParserT m String Char
satisfy p = PT f
  where
    f s with (strM s)
      f "" | StrNil
          = pure . fail "" $ "not eof"
      f (strCons x xs) | StrCons x xs
          = case p x of
            True  => pure $ Success xs x
            False => pure . fail (strCons x xs) $ "not '" ++ c2s x ++ "'"

char : Monad m => Char -> ParserT m String ()
char c = skip (satisfy (== c)) <?-> "character '" ++ c2s c ++ "'"

string : Monad m => String -> ParserT m String ()
string s = traverse_ char (unpack s) <?-> "string " ++ show s

many : Monad m => ParserT m str a -> ParserT m str (List a)
many p = [| p :: many p |] <|> pure []

some : Monad m => ParserT m str a -> ParserT m str (List a)
some p = [| p :: many p |]

space : Monad m => ParserT m String ()
space = skip . many . satisfy $ isSpace <?-> "whitespace"

token : Monad m => String -> ParserT m String ()
token s = skip (string s) <$ space <?-> "token " ++ show s

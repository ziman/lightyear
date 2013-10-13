module Lightyear.Core

data Tag =
    Lib   -- syntax elements from this library
  | User  -- solely user-defined elements (none found here)
  | Mod   -- modifiers/extenders, like many, some, parens, ...
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

-- should take a noun: (many) "element"
infixl 0 <?>
(<?>) : Monad m => ParserT m str a -> String -> ParserT m str a
p <?> msg = p <??> (User, msg)

-- should take an adjective: "many" (elements)
infixl 0 <?+>
(<?+>) : Monad m => ParserT m str a -> String -> ParserT m str a
p <?+> msg = p <??> (Mod, msg)

c2s : Char -> String
c2s c = pack (c :: [])

skip : Functor f => f a -> f ()
skip = map (const ())

record Stream : Type -> Type -> Type where
  St : (uncons : str -> Maybe (tok, str)) -> Stream tok str

satisfy' : (Monad m) => Stream tok str -> (tok -> Bool) -> ParserT m str tok
satisfy' (St uncons) p = PT $ \s => pure $ case uncons s of
  Nothing => fail s $ "<???-1>"
  Just (t, s') => case p t of
    True  => Success s' t
    False => fail s $ "<???-2>"

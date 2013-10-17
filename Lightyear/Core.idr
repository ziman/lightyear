module Lightyear.Core

%access public

data Tag =
    Lib   -- syntax elements from this library
  | User  -- solely user-defined elements (none found here)
  | Mod   -- modifiers/extenders, like many, some, parens, ...

instance Show Tag where
  show Lib  = "Lib"
  show User = "User"
  show Mod  = "Mod"

data Result str a =
    Success str a
  | Failure (List (Tag, str, String)) -- a stacktrace of errors based on <??> and friends

instance Functor (Result str) where
  map f (Success s x) = Success s (f x)
  map f (Failure es ) = Failure es

record ParserT : (m : Type -> Type) -> (str : Type) -> (a : Type) -> Type where
  PT : (runParserT : str -> m (Result str a)) -> ParserT m str a

instance Monad m => Functor (ParserT m str) where
  map f (PT p) = PT (map (map f) . p)

-- Lazy variant of <$>.
infixl 2 <$*>
(<$*>) : Monad m => ParserT m str (a -> b) -> |(x : ParserT m str a) -> ParserT m str b
(<$*>) (PT f) x = PT $ \s => f s >>= \f' => case f' of
  Failure es   => pure (Failure es)
  Success s' g => let PT x' = x in map (map g) (x' s')

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\s => pure (Success s x))
  (<$>) = (<$*>)

instance Monad m => Monad (ParserT m str) where
  (>>=) (PT x) f = PT $ \s => x s >>= \r => case r of
    Failure es   => pure (Failure es)
    Success s' y => let PT f' = f y in f' s'

fail : str -> String -> Result str a
fail s msg = Failure ((Lib, s, msg) :: [])

-- include error stack from the other alternative branch
altError : Monad m => List (Tag, str, String) -> Result str a -> Result str a
altError es (Success s x) = Success s x
altError es (Failure es') = Failure es'  -- TODO

-- Lazy variant of <|>.
infixl 3 <|*>
(<|*>) : Monad m => ParserT m str a -> |(y : ParserT m str a) -> ParserT m str a
(<|*>) (PT x) y = PT $ \s => x s >>= \r => case r of
  Success s' x' => pure (Success s' x')
  Failure es    => let PT y' = y in map (altError es) (y' s)

instance Monad m => Alternative (ParserT m str) where
  empty = PT (\s => pure . fail s $ "non-empty alternative")
  (<|>) = (<|*>)

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

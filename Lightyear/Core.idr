module Lightyear.Core

-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.

%access public

data Commitment = Committed | Uncommitted

data Result str a =
    Success str a
  | Failure Commitment (List (str, String)) -- a stacktrace of errors based on <?> and friends

instance Functor (Result str) where
  map f (Success s x ) = Success s (f x)
  map f (Failure c es) = Failure c es

record ParserT : (m : Type -> Type) -> (str : Type) -> (a : Type) -> Type where
  PT : (runParserT : str -> m (Result str a)) -> ParserT m str a

instance Monad m => Functor (ParserT m str) where
  map f (PT p) = PT (map (map f) . p)

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\s => pure (Success s x))

  -- Beware! the following function must NOT pattern-match
  -- on its second argument right away because it might be lazy.
  (<$>) (PT f) x = PT $ \s => f s >>= \f' => case f' of
    Failure c es => pure (Failure c es)
    Success s' g => let PT x' = x in map (map g) (x' s')

instance Monad m => Monad (ParserT m str) where
  (>>=) (PT x) f = PT $ \s => x s >>= \r => case r of
    Failure c es => pure (Failure c es)
    Success s' y => let PT f' = f y in f' s'

fail : str -> String -> Result str a
fail s msg = Failure Uncommitted ((s, msg) :: [])

instance Monad m => Alternative (ParserT m str) where
  empty = PT (\s => pure . fail s $ "non-empty alternative")

  -- Beware! the following function must NOT pattern-match
  -- on its second argument right away because it might be lazy.
  (<|>) (PT x) y = PT $ \s => x s >>= \r => case r of
    Success s' x' => pure (Success s' x')
    Failure Committed   es => pure $ Failure Committed es
    Failure Uncommitted es => let PT y' = y in y' s

infixl 0 <?>
(<?>) : Monad m => ParserT m str a -> String -> ParserT m str a
(PT f) <?> msg = PT $ \s => map (mogrify s) (f s)
  where
    mogrify : str ->  Result str a -> Result str a
    mogrify s (Failure c  es) = Failure c ((s, msg) :: es)
    mogrify s (Success s' x ) = Success s' x

commitTo : Monad m => ParserT m str a -> ParserT m str a
commitTo (PT f) = PT (map mogrify . f)
  where
    mogrify : Result str a -> Result str a
    mogrify (Success s x ) = Success s x
    mogrify (Failure _ es) = Failure Committed es

record Stream : Type -> Type -> Type where
  St : (uncons : str -> Maybe (tok, str)) -> Stream tok str

satisfy' : Monad m => Stream tok str -> (tok -> Bool) -> ParserT m str tok
satisfy' (St uncons) p = PT $ \s => pure $ case uncons s of
  Nothing => fail s $ "a character, not EOF"
  Just (t, s') => case p t of
    True  => Success s' t
    False => fail s $ "a different character"

satisfyMaybe' : Monad m => Stream tok str -> (tok -> Maybe out) -> ParserT m str out
satisfyMaybe' (St uncons) f = PT $ \s => pure $ case uncons s of
  Nothing => fail s $ "a character, not EOF"
  Just (t, s') => case f t of
    Just res => Success s' res
    Nothing => fail s $ "a different character"

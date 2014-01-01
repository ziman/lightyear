module Lightyear.Core

-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.

%access public

data Result str a =
    Success str a
  | Failure (List (str, String)) -- a stacktrace of errors based on <?> and friends

instance Functor (Result str) where
  map f (Success s x ) = Success s (f x)
  map f (Failure es) = Failure es

record ParserT : (m : Type -> Type) -> (str : Type) -> (a : Type) -> Type where
  PT : (runParserT : (r : Type) ->
                     (a -> str -> m r) -> -- uncommitted success
                     (a -> str -> m r) -> -- committed success
                     (List (str, String) -> m r) -> -- uncommitted error
                     (List (str, String) -> m r) -> -- committed error
                     str -> m r) -> ParserT m str a

execParserT : Monad m => ParserT m str a -> str -> m (Result str a)
execParserT {m} {str} {a} (PT p) i = p (Result str a) success success failure failure i
  where success x i = pure $ Success i x
        failure = pure . Failure

instance Monad m => Functor (ParserT m str) where
  map {a} {b} f (PT p) = PT $ \r => \us => \cs => p r (us . f) (cs . f)

instance Monad m => Applicative (ParserT m str) where
  pure x = PT (\r => \us => \cs => \ue => \ce => us x)

  -- Beware! the following function must NOT pattern-match
  -- on its second argument right away because it might be lazy.
  (<$>) (PT f) x = PT $ \r => \us => \cs => \ue => \ce =>
    f r (\f' => let PT g = x in g r (us . f') (cs . f') ue ce)
        (\f' => let PT g = x in g r (cs . f') (cs . f') ce ce)
        ue ce

instance Monad m => Monad (ParserT m str) where
  (>>=) (PT x) f = PT $ \r => \us => \cs => \ue => \ce =>
    x r (\x' => let PT y = f x' in y r us cs ue ce)
        (\x' => let PT y = f x' in y r cs cs ce ce)
        ue ce

fail : String -> ParserT m str a
fail msg = PT $ \r => \us => \cs => \ue => \ce => \i => ue [(i, msg)]

instance Monad m => Alternative (ParserT m str) where
  empty = fail "non-empty alternative"

  -- Beware! the following function must NOT pattern-match
  -- on its second argument right away because it might be lazy.
  (<|>) (PT x) y = PT $ \r => \us => \cs => \ue => \ce => \i =>
    x r us cs (\err => let PT y' = y in y' r us cs (ue . (err ++))
                                                   (ce . (err ++)) i) ce i

infixl 0 <?>
(<?>) : Monad m => ParserT m str a -> String -> ParserT m str a
(PT f) <?> msg = PT $ \r => \us => \cs => \ue => \ce => \i =>
  f r us cs (ue . ((i, msg) ::)) (ce . ((i, msg) ::)) i

commitTo : Monad m => ParserT m str a -> ParserT m str a
commitTo (PT f) = PT $ \r => \us => \cs => \ue => \ce => f r cs cs ce ce

record Stream : Type -> Type -> Type where
  St : (uncons : str -> Maybe (tok, str)) -> Stream tok str

satisfyMaybe' : Monad m => Stream tok str -> (tok -> Maybe out) -> ParserT m str out
satisfyMaybe' (St uncons) f = PT $ \r => \us => \cs => \ue => \ce => \i =>
  case uncons i of
    Nothing      => ue [(i, "a token, not EOF")]
    Just (t, i') => case f t of
      Nothing  => ue [(i, "a different token")]
      Just res => us res i'

satisfy' : Monad m => Stream tok str -> (tok -> Bool) -> ParserT m str tok
satisfy' st p = satisfyMaybe' st (\t => if p t then Just t else Nothing)

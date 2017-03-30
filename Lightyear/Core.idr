-- ---------------------------------------------------------------- [ Core.idr ]
-- Module      : Lightyear.Core
-- Description : Central Definitions and Instances
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Core

import Data.Fin
import Control.Monad.Trans
import Control.Monad.State

%access export
%default total

||| Parse results
public export
data Result str a =
  ||| Sucess, returning the remaining string and the parser result
  Success str a |
  ||| Failure, returning a stack trace of errors based on `<?>`
  Failure (List (str, String)) -- a stacktrace of errors based on <?> and friends

implementation Functor (Result str) where
  map f (Success s x ) = Success s (f x)
  map f (Failure es) = Failure es

record ParserT str (m : Type -> Type) a where
  constructor PT
  runParserT :
    (r : Type) ->
    (a -> str -> m r) -> -- uncommitted success
    (a -> str -> m r) -> -- committed success
    (List (str, String) -> m r) -> -- uncommitted error
    (List (str, String) -> m r) -> -- committed error
    str ->
    m r

||| Run a parser monad on some input
execParserT : Monad m => ParserT str m a
                      -> (input : str)
                      -> m (Result str a)
execParserT {str} {m} {a} (PT p) input = p (Result str a) success success failure failure input
  where success x i = pure $ Success i x
        failure = pure . Failure

implementation Monad m => Functor (ParserT str m) where
  map {a} {b} f (PT p) = PT $ \r, us, cs => p r (us . f) (cs . f)

implementation Monad m => Applicative (ParserT str m) where
  pure x = PT (\r, us, cs, ue, ce => us x)

  (<*>) (PT f) (PT g) = PT $ \r, us, cs, ue, ce =>
    f r (\f' => g r (us . f') (cs . f') ue ce)
        (\f' => g r (cs . f') (cs . f') ce ce)
        ue ce


infixl 2 <*>|
||| A variant of <*>, lazy in its second argument, which must NOT be
||| pattern-matched right away because we want to keep it lazy in case
||| it's not used.
(<*>|) : Monad m => ParserT str m (a -> b)
                 -> Lazy (ParserT str m a)
                 -> ParserT str m b
(<*>|) (PT f) x = PT $ \r, us, cs, ue, ce =>
    f r (\f' => let PT g = x in g r (us . f') (cs . f') ue ce)
        (\f' => let PT g = x in g r (cs . f') (cs . f') ce ce)
        ue ce

implementation Monad m => Monad (ParserT str m) where
  (>>=) (PT x) f = PT $ \r, us, cs, ue, ce =>
    x r (\x' => let PT y = f x' in y r us cs ue ce)
        (\x' => let PT y = f x' in y r cs cs ce ce)
        ue ce

implementation MonadTrans (ParserT str) where
  lift x = PT $ \r, us, cs, ue, ce, s => (x >>= flip us s)

implementation MonadState s m => MonadState s (ParserT str m) where
  get = lift get
  put = lift . put

||| Fail with some error message
fail : String -> ParserT str m a
fail msg = PT $ \r, us, cs, ue, ce, i => ue [(i, msg)]

implementation Monad m => Alternative (ParserT str m) where
  empty = fail "non-empty alternative"

  (<|>) (PT x) (PT y) = PT $ \r, us, cs, ue, ce, i =>
    x r us cs (\err => y r us cs (ue . (err ++))
                                 (ce . (err ++)) i) ce i

infixl 3 <|>|

||| A variant of <|>, lazy in its second argument, which must NOT be
||| pattern-matched right away because we want to keep it lazy in case
||| it's not used.
(<|>|) : Monad m => ParserT str m a
                 -> Lazy (ParserT str m a)
                 -> ParserT str m a
(<|>|) (PT x) y = PT $ \r, us, cs, ue, ce, i =>
  x r us cs (\err => let PT y' = y in y' r us cs (ue . (err ++))
                                                 (ce . (err ++)) i) ce i

infixl 0 <?>
||| Associate an error with parse failure
(<?>) : Monad m => ParserT str m a -> String -> ParserT str m a
(PT f) <?> msg = PT $ \r, us, cs, ue, ce, i =>
  f r us cs (ue . ((i, msg) ::)) (ce . ((i, msg) ::)) i

||| Commit to a parse alternative and prevent backtracking
commitTo : Monad m => ParserT str m a -> ParserT str m a
commitTo (PT f) = PT $ \r, us, cs, ue, ce => f r cs cs ce ce

-- There is no reason that we mark "str" as the determining type
-- other than to aid typeclass/interface resolution.
--
-- I feel that having this restriction (which is probably okay
-- given that the only streams so far are String and Text anyway)
-- is more acceptable than failing surprisingly
-- any time the unsuspecting user calls "satisfy" without {tok=Char}
-- in an odd context.
--
-- We make "str" the determining type because it's usually fixed
-- by the parser monad you're working in, which helps resolution.
interface Stream tok str | str where
  uncons : str -> Maybe (tok, str)

||| Matches a single element that satisfies some condition, accepting
||| a transformation of successes.
satisfyMaybe : (Monad m, Stream tok str)
                        => (tok -> Maybe out)
                        -> ParserT str m out
satisfyMaybe {tok=tok} {str=str} f =
  PT $ \r, us, cs, ue, ce, i =>
    case uncons {tok=tok} {str=str} i of
      Nothing      => ue [(i, "a token, not EOF")]
      Just (t, i') => case f t of
        Nothing  => ue [(i, "a different token")]
        Just res => us res i'

||| Matches a single element that satisfies some condition.
satisfy : (Monad m, Stream tok str)
                   => (tok -> Bool)
                   -> ParserT str m tok
satisfy p = satisfyMaybe (\t => if p t then Just t else Nothing)

||| Succeeds if and only if the argument parser fails.
|||
||| In Parsec, this combinator is called `notFollowedBy`.
requireFailure : ParserT str m tok -> ParserT str m ()
requireFailure (PT f) = PT $ \r, us, cs, ue, ce, i =>
                               f r
                                 (\t, s => ue [(i, "argument parser to fail")])
                                 (\t, s => ce [(i, "argument parser to fail")])
                                 (\errs => us () i)
                                 (\errs => cs () i)
                                 i
-- --------------------------------------------------------------------- [ EOF ]

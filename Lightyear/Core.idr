--- ---------------------------------------------------------------- [ Core.idr ]
--- Module      : Lightyear.Core
--- Description : Central Definitions and Instances
---
--- This code is distributed under the BSD 2-clause license.
--- See the file LICENSE in the root directory for its full text.
--- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Core

import Control.Monad.Trans
import Control.Monad.State

import Lightyear.Position

import Debug.Trace

%access public export

namespace Lightyear.State

  ||| State to keep track off during parsing.
  record State (str : Type) where
    constructor ST
    input     : str
    position  : Position
    tab_width : Nat

namespace Lightyear.Error

  ||| Error messages to return.
  record Error str where
    constructor Err
    position : Position
    msg : String

  display : Error str -> String
  display (Err pos msg) = concat [display pos,":\n\t", msg]

data Result : (str : Type) -> (a : Type) -> Type where
  Success : a -> Result str a
  Failure : List (Error str) -> Result str a

Functor (Result str) where
  map f (Success x)  = Success (f x)
  map f (Failure es) = Failure es

data Reply : (str : Type) -> (a : Type) -> Type where
  MkReply : State str -> Result str a -> Reply str a

record ParserT (str : Type) (m : Type -> Type) a where
  constructor PT
  runParserT : (r : Type)
            -> (parseOK  : a -> State str -> m r)
            -> (emptyOK  : a -> State str -> m r)
            -> (parseErr : List (Error str) -> State str -> m r)
            -> (emptyErr : List (Error str) -> State str -> m r)
            -> State str
            -> m r


execParserT : Monad m
           => (parser : ParserT str m a)
           -> (input  : State str)
           -> m (Reply str a)
execParserT {str} {m} {a} (PT p) input =
    p (Reply str a) success success failure failure input
   where
     success : a -> State str -> m (Reply str a)
     success i x = pure $ MkReply x $ Success i

     failure : List (Error str) -> State str -> m (Reply str a)
     failure es x = pure $ MkReply x $ Failure es


Monad m => Functor (ParserT str m) where
  map {a} {b} f (PT p) = PT $ \r, us, cs => p r (us . f) (cs . f)

Monad m => Applicative (ParserT str m) where
  pure x = PT (\r, us, cs, ue, ce => us x)

  (<*>) (PT f) (PT g) =
      PT $ \r, us, cs, ue, ce
         => f r (\f' => g r (us . f') (cs . f') ue ce)
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

Monad m => Monad (ParserT str m) where
  (>>=) (PT x) f =
      PT (\r, us, cs, ue, ce
            => x r (\x' => let PT y = f x' in y r us cs ue ce)
                   (\x' => let PT y = f x' in y r cs cs ce ce)
                   ue ce)

MonadTrans (ParserT str) where
  lift x = PT (\r, us, cs, ue, ce, s => (x >>= flip us s))

MonadState s m => MonadState s (ParserT str m) where
  get = lift get
  put = lift . put

||| Fail with some error message
fail : String -> ParserT str m a
fail msg = PT $ \r, us, cs, ue, ce, (ST i p tw) => ue [Err p msg] (ST i p tw)

Monad m => Alternative (ParserT str m) where
  empty = fail "non-empty alternative"

  (<|>) (PT left) (PT right) =
      PT $ \r, us, cs, ue, ce, st =>
          left r us cs (\err,st' => right r us cs (ue . (err ++))
                                                  (ce . (err ++)) st) ce st


lightyearAlt' : Monad m
             => (left  : ParserT str m a)
             -> (right : Lazy $ ParserT str m a)
             -> ParserT str m a
lightyearAlt' (PT left) right = PT leftright
  where
    leftright : (r : Type)
             -> (parseOK  : a -> State str -> m r)
             -> (emptyOK  : a -> State str -> m r)
             -> (parseErr : List (Error str) -> State str -> m r)
             -> (emptyErr : List (Error str) -> State str -> m r)
             -> State str
             -> m r
    leftright r us cs ue ce st =
        left r us cs (\err, st' => let PT right' = right
                                    in right' r us cs (ue . (err++))
                                                      (ce . (err++)) st)
                                                      ce st

infixl 3 <|>|

||| A variant of <|>, lazy in its second argument, which must NOT be
||| pattern-matched right away because we want to keep it lazy in case
||| it's not used.
(<|>|) : Monad m => ParserT str m a
                 -> Lazy (ParserT str m a)
                 -> ParserT str m a
(<|>|) = lightyearAlt'


infixl 0 <?>
||| Associate an error with parse failure
(<?>) : Monad m => ParserT str m a -> String -> ParserT str m a
(<?>) (PT f) msg = PT $ \r, us, cs, ue, ce, (ST i pos tw)
    => f r us cs (ue . ((Err pos msg)::))
                 (ce . ((Err pos msg)::)) (ST i pos tw)

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

  updatePos : Nat -> Position -> tok -> Pair Position Position


initialState : Maybe String -> s -> Nat -> State s
initialState name s tw = ST s (defaultPos name) tw

||| Matches a single element that satisfies some condition, accepting
||| a transformation of successes.
satisfyMaybe : (Monad m, Stream tok str)
            => (tok -> Maybe out)
            -> ParserT str m out
satisfyMaybe {tok=tok} {str=str} f =
   PT $ \r, us, cs, ue, ce, (ST i pos tw) =>
     case uncons {tok=tok} {str=str} i of
       Nothing        => ue [Err pos "a token, not EOF"] (ST i pos tw)
       Just (t, rest) => case f t of
         Nothing  => ue [Err pos "a different token"] (ST i pos tw)
         Just res => let (newPos, _) = updatePos {tok=tok} {str=str} tw pos t
                      in us res (ST rest newPos tw)

||| Matches a single element that satisfies some condition.
satisfy : (Monad m, Stream tok str)
                   => (tok -> Bool)
                   -> ParserT str m tok
satisfy p = satisfyMaybe (\t => if p t then Just t else Nothing)


||| Succeeds if and only if the argument parser fails.
|||
||| In Parsec, this combinator is called `notFollowedBy`.
requireFailure : ParserT str m tok -> ParserT str m ()
requireFailure (PT f) = PT $ \r, us, cs, ue, ce, (ST i pos tw) =>
                               f r
                                 (\t, s => ue [Err pos "argument parser to fail"] s)
                                 (\t, s => ce [Err pos "argument parser to fail"] s)
                                 (\errs, s => us () s)
                                 (\errs, s => cs () s)
                                 (ST i pos tw)


interface (Monad m, Stream tok str) => ParserM tok str (m : Type -> Type) | m where
  getState : m (State str)

(Monad m, Stream tok str) => ParserM tok str (ParserT str m) where
  getState = PT $ \r,us,cs,ue,ce,s => cs s s

||| Return the current position of the parser in the input stream.
getPosition : ParserM tok str m => m Position
getPosition = do
  st <- getState
  pure (position st)

-- --------------------------------------------------------------------- [ EOF ]

-- ------------------------------------------------------------ [ Position.idr ]
-- Module    : Lightyear.Position
-- Description : Positioning in source files.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]
module Lightyear.Position

import Data.String

%default total
%access export

||| Generic representation of a position.
public export
record Position where
  constructor MkPos
  fname   : Maybe String
  lineNo  : Nat
  colNo   : Nat

defaultPos : Maybe String -> Position
defaultPos fname = MkPos fname 1 1

namespace Generic
  ||| Increment the position one character.
  partial
  increment : (tabwidth  : Nat)
           -> (curr_pos  : Position)
           -> (character : Char)
           -> Position
  increment _      (MkPos f l _) '\n' = MkPos f (S l) 1
  increment twidth (MkPos f l c) '\t' = MkPos f l     nextTab
    where
      partial
      nextTab : Nat
      nextTab = (div ((minus c 1) + twidth) twidth) * twidth + 1

  increment _ (MkPos f l c) _ = MkPos f l (S c)

  ||| Increment the position one character.
  partial
  inc : Nat -> Position -> Char -> Position
  inc = Generic.increment

||| Increment the position one character with a default tab width of eight.
partial
increment : Position -> Char -> Position
increment = Generic.increment 8

||| Increment the position one character with a default tab width of eight.
partial
inc : Position -> Char -> Position
inc = Generic.increment 8

-- [ NOTE ]
--
-- When comparing ASTs it would be nice to ignore positioning information.

Eq Position where
  (==) _ _ = True

Ord Position where
  compare _ _ = EQ

display : Position -> String
display (MkPos (Just fname) l c) = concat [show fname, ":", show l, ":", show c]
display (MkPos Nothing      l c) = concat ["At ", show l, ":", show c]

-- --------------------------------------------------------------------- [ EOF ]

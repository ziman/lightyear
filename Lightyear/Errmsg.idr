-- -------------------------------------------------------------- [ Errmsg.idr ]
-- Module      : Lightyear.Errmsg
-- Description : Error message formatting.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]
module Errmsg

import Lightyear.Core

-- ------------------------------------------------------------------- [ Begin ]
public
class Layout str where
  lineLengths : str -> List Int

nat2int : Nat -> Int
nat2int  Z    = 0
nat2int (S x) = 1 + nat2int x

rowcol : List Int -> List Int -> (Int, Int)
rowcol       ws        []  = (0,0)  -- should not happen
rowcol (w :: ws) (x :: []) = (1 + (nat2int $ length ws), 1 + w-x)
rowcol (w :: ws) (x :: xs) = rowcol ws xs
rowcol       []        xs  = (0,0)  -- should not happen

formatItem : Layout str => str
                        -> (str, String)
                        -> String
formatItem whole (rest, msg)
  = let (row, col) = rowcol (reverse $ lineLengths whole) (reverse $ lineLengths rest)
    in "at " ++ show row ++ ":" ++ show col ++ " expected:\n  " ++ msg

namespace Err
  unlines : List String -> String
  unlines []  = ""
  unlines [s] = s
  unlines (s :: ss) = s ++ "\n" ++ Err.unlines ss

public
formatError : Layout str => str
                         -> List (str, String)
                         -> String
formatError whole = Err.unlines . map (formatItem whole)
-- --------------------------------------------------------------------- [ EOF ]

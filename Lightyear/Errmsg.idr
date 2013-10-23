module Errmsg

import Lightyear.Core

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

formatItem : Layout str => str -> (str, String) -> String
formatItem whole (rest, msg)
  = let (row, col) = rowcol (reverse $ lineLengths whole) (reverse $ lineLengths rest)
    in "at " ++ show row ++ ":" ++ show col ++ ":\n" ++ msg ++ "\n"

public
formatError : Layout str => str -> List (str, String) -> String
formatError whole = concat . map (formatItem whole)

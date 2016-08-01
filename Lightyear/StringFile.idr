-- -------------------------------------------------------------- [ StringFile ]
-- Module      : Lightyear.StringFile
-- Description : String-related effectful file parsers.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
-- --------------------------------------------------------------------- [ EOH ]

module Lightyear.StringFile

import public Effects
import public Effect.File

import Lightyear
import Lightyear.Strings

%access export
-- ---------------------------------------------------- [ A String File Parser ]

||| Parse a file using the specified parser.
|||
||| @rErr  A custom error for reporting FileIO errors.
||| @pErr  A custom error for reporting Parsing errors.
||| @p     The parser to use on the specified file.
||| @fname The name of the file to parser.
parseFile : (rErr  : String -> FileError -> e)
         -> (pErr  : String -> String -> e)
         -> (p     : Parser a)
         -> (fname : String)
         -> Eff (Either e a) [FILE ()]
parseFile rErr pErr p fn = do
    Result src <- readFile fn
                  | FError err => pure (Left (rErr fn err))
    case parse p src of
      Left err  => pure $ Left (pErr fn err)
      Right res => pure $ Right res


-- --------------------------------------------------------------------- [ EOF ]

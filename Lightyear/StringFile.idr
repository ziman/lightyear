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

namespace Lightyear
  private
  readFile : (String -> e) -> String -> Eff (Either e String) [FILE_IO ()]
  readFile errFunc fname = do
      case !(open fname Read) of
        False => pure $ Left (errFunc fname)
        True => do
          src <- readAcc ""
          close
          pure $ Right src
    where
      readAcc : String -> Eff String [FILE_IO (OpenFile Read)]
      readAcc acc = if (not !(eof))
                       then readAcc (acc ++ !(readLine))
                       else pure acc


parseFile : (String -> e)
         -> (String -> String -> e)
         -> Parser a
         -> String
         -> Eff (Either e a) [FILE_IO ()]
parseFile rErr pErr p fn = do
    Right src <- Lightyear.readFile rErr fn | Left err => pure (Left err)
    case parse p src of
      Left err  => pure $ Left (pErr fn err)
      Right res => pure $ Right res


-- --------------------------------------------------------------------- [ EOF ]

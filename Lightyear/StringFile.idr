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

-- ---------------------------------------------------- [ A String File Parser ]

parseFile : Parser a
         -> String
         -> Eff (Either String a) [FILE_IO ()]
parseFile p fn =
    case !(open fn Read) of
      True => do
        src <- doRead ""
        close
        pure $ parse p src
      False => pure $ Left "Could Not Open File"
  where
    doRead : String -> Eff String [FILE_IO (OpenFile Read)]
    doRead b = if (not !eof)
                   then doRead (b ++ !readLine)
                   else pure b


-- --------------------------------------------------------------------- [ EOF ]

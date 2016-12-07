module JsonTest

import Json

import Lightyear.Testing

%access export

test : String -> IO ()
test s = case parse jsonToplevelValue s of
  Left err => putStrLn $ "error: " ++ err
  Right v  => printLn v

jsonTests : IO ()
jsonTests = runTests
  [ parseTestCmpShow
      "JSON 1"
      jsonToplevelValue
      "[1,2,4,[5,6],null,{\"some\":[\"object\"]},false]"
      "[1, 2, 4, [5, 6], null, {\"some\": [\"object\"]}, false]"
  , parseTestCmpShow
      "JSON 2"
      jsonToplevelValue
      "{\n  \"hallo\":42,\"nichts\":null}"
      "{\"hallo\": 42, \"nichts\": null}"

  , parseTestCmpShow
      "JSON 3"
      jsonToplevelValue
      "{\"hello\": [{\"world\": false}, 3, \"string\", true, null]}"
      "{\"hello\": [{\"world\": false}, 3, \"string\", true, null]}"

  , parseTestCmpNot
      "JSON 4"
      jsonToplevelValue
      "{{\n  \"hallo\":42,\"nichts\":null}"
      """at 1:1 expected:
  character '['
at 1:1 expected:
  a different token
at 1:2 expected:
  character '}'
at 1:2 expected:
  a different token
"""
  ]

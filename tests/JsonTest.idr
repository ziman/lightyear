module Main

import Json

test : String -> IO ()
test s = case parse jsonToplevelValue s of
  Left err => putStrLn $ "error: " ++ err
  Right v  => print v

main : IO ()
main = do
  test "{\n  \"hallo\":42,\"nichts\":null}"
  test "{{\n  \"hallo\":42,\"nichts\":null}"
  test "{\"hello\": [{\"world\": false}, 3, \"string\", true, null]}"

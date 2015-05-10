module Main

import Json

test : String -> IO ()
test s = case parse jsonToplevelValue s of
  Left err => putStrLn $ "error: " ++ err
  Right v  => printLn v

main : IO ()
main = do
  test "[1,2,4,[5,6],null,{\"some\":[\"object\"]},false]"
  test "{\n  \"hallo\":42,\"nichts\":null}"
  test "{{\n  \"hallo\":42,\"nichts\":null}"
  test "{\"hello\": [{\"world\": false}, 3, \"string\", true, null]}"

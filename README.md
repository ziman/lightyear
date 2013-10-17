# Lightyear

Parser combinator library for Idris,
inspired by [Parsec](http://hackage.haskell.org/package/parsec).

Module overview:
* `Lightyear.Core`: central definitions + instances
* `Lightyear.Combinator`: generic combinators like `many` or `sepBy`
* `Lightyear.String_`: string-bound parsers like `char` or `space`

## Build
```bash
$ #idris --clean lightyear.idr
$ idris --build lightyear.idr
$ idris --install lightyear.idr
```

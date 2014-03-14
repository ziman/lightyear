#!/bin/bash

die() {
	echo "$1" >&2
	exit 1
}

rm -f *.ibc test

idris Test.idr -p lightyear -o test || die "could not compile tests"
./test || die "could not run tests"

rm -f *.ibc test

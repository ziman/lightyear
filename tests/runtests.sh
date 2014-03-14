#!/bin/bash

die() {
	echo "$1" >&2
	exit 1
}

rm -f *.ibc test output

idris Test.idr -p lightyear -o test || die "could not compile tests"
timeout 5s ./test > output || die "test failed or timed out"

if diff output expected; then
	echo "### PASS ###"
else
	echo "### FAIL ###"
fi

rm -f *.ibc test output

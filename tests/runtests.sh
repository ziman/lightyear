#!/bin/bash

die() {
	echo "$1" >&2
	exit 1
}

clean_up() {
	rm -f *.ibc test json output
}

clean_up

echo "compiling lightyear tests..."
idris Test.idr -p lightyear -o test --no-partial-eval || die "* could not compile tests *"

echo "compiled OK, running lightyear tests..."
timeout 5s ./test > output || die "* test failed or timed out *"

echo "compiling the JSON test..."
idris JsonTest.idr -p lightyear -o json --no-partial-eval || die "* could not compile the json test *"

echo "compiled OK, running the JSON test..."
timeout 5s ./json >> output || die "* test failed or timed out *"

if diff output expected; then
	echo "### everything PASS ###"
	clean_up
	exit 0
else
	echo "### something FAIL ###"
	clean_up
	exit 1
fi

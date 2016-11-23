#!/usr/bin/env bash

set -euo pipefail

OS=`uname -s`

# Timeout on Mac OS X is installed via brew install coreutils and called gtimeout
if [ "$OS" = "Darwin" ]; then
	TIMEOUTCMD=gtimeout
else
	TIMEOUTCMD=timeout
fi

echo "TIMOUTCMD set to ${TIMEOUTCMD}"

die() {
	echo "$1" >&2
	exit 1
}

clean_up() {
	rm -f *.ibc test json output
}

clean_up

echo "compiling lightyear tests..."
idris Test.idr -p lightyear -o test || die "* could not compile tests *"

echo "compiled OK, running lightyear tests..."
$TIMEOUTCMD 5s ./test > output || die "* test failed or timed out *"

echo "compiling the JSON test..."
idris JsonTest.idr -p lightyear -p contrib -o json || die "* could not compile the json test *"

echo "compiled OK, running the JSON test..."
$TIMEOUTCMD 5s ./json >> output || die "* test failed or timed out *"

# use a default parameter if $1 is not set
if [ "${1-}" = "believe_me" ]; then
	echo '### marking current output as expected ###'
	cat output > expected
	clean_up
	exit 0
else
	if diff output expected; then
		echo "### everything PASS ###"
		clean_up
		exit 0
	else
		echo "### something FAIL ###"
		clean_up
		exit 1
	fi
fi

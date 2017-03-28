all: install

install: build
	idris --install lightyear.ipkg

build: Lightyear/*.idr
	idris --build lightyear.ipkg

test: build
	(cd tests; bash runtests.sh)

clean:
	idris --clean lightyear.ipkg
	rm -f tests/*.ibc

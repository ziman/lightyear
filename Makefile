all: install

install: build
	idris --install lightyear.ipkg

build: Lightyear/*.idr
	idris --build lightyear.ipkg

test:
	(cd tests; bash runtests.sh)

clean:
	idris --clean lightyear.ipkg

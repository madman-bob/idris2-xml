.PHONY: test retest clean

INTERACTIVE ?= --interactive
threads ?= $(shell (nproc || sysctl -n hw.ncpu) 2>/dev/null || echo 1)

test: build/exec/xml-tests
	./build/exec/xml-tests idris2 $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only $(only)

retest: build/exec/xml-tests
	./build/exec/xml-tests idris2 $(INTERACTIVE) --timing --failure-file failures --threads $(threads) --only-file failures --only $(only)

build/exec/xml-tests: xml-tests.ipkg XMLTests.idr
	idris2 --build xml-tests.ipkg

clean:
	$(RM) failures
	$(RM) -r build
	@find . -type f -name 'output' -exec rm -rf {} \;

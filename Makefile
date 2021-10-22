.PHONY: all install xml test retest clean

all: xml

install: xml
	idris2 --install xml.ipkg

xml: build/ttc/Language/XML.ttc

build/ttc/Language/XML.ttc: xml.ipkg XML/* XML/*/* XML/*/*/*
	idris2 --build xml.ipkg

test:
	make -C tests test

retest:
	make -C tests retest

clean:
	rm -rf build

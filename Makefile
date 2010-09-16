.PHONY: all
all: derive

PACKAGEDB=package.conf.d

ifneq (,$(findstring CYGWIN, $(ARCH)))
PREFIX_PATH:=$(shell cygpath.exe -m -w $(PWD))
PACKAGEDB_PATH:=$(shell cygpath.exe -m -w $(abspath $(PACKAGEDB)))
else
PREFIX_PATH=$(PWD)
PACKAGEDB_PATH=$(abspath $(PACKAGEDB))
endif

.PHONY: packagedb
packagedb: $(PACKAGEDB)
$(PACKAGEDB):
	ghc-pkg init $(PACKAGEDB)

install-local:
	cd netlist && cabal install --prefix=$(PREFIX_PATH) --package-db=$(PACKAGEDB_PATH)
	cd netlist-to-vhdl && cabal install --prefix=$(PREFIX_PATH) --package-db=$(PACKAGEDB_PATH)
	cd verilog && cabal install --prefix=$(PREFIX_PATH) --package-db=$(PACKAGEDB_PATH)
	cd netlist-to-verilog && cabal install --prefix=$(PREFIX_PATH) --package-db=$(PACKAGEDB_PATH)

.PHONY: derive
# run derive on every file that has derived instances in it.
# generated boilerplate code for instances of Binary.
.PHONY: derive
derive:
	@for f in `grep -l OPTIONS_DERIVE -r --include=*.hs .`; do \
            echo derive $$f; derive $$f; \
            sed -i -e 's/[ \t]*$$//g' $$f; done

.PHONY: delete-trailing-whitespace
delete-trailing-whitespace:
	@for f in `/usr/bin/find . -iname \*.hs`; do \
            sed -i -e 's/[ \t]*$$//g' $$f; done

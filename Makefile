.PHONY: all
all: derive

PACKAGEDB=package.conf.d

.PHONY: packagedb
packagedb: $(PACKAGEDB)
$(PACKAGEDB):
	ghc-pkg init $(PACKAGEDB)

install-local:
	cd netlist && cabal install --prefix=$(PWD) --package-db=../$(PACKAGEDB)
	cd netlist-to-vhdl && cabal install --prefix=$(PWD) --package-db=../$(PACKAGEDB)
	cd verilog && cabal install --prefix=$(PWD) --package-db=../$(PACKAGEDB)
	cd netlist-to-verilog && cabal install --prefix=$(PWD) --package-db=../$(PACKAGEDB)

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


hudson:
	cd netlist && cabal install
	cd netlist-to-vhdl && cabal install

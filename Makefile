.PHONY: all
all: derive

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

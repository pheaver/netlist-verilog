.PHONY: all
all: derive

.PHONY: derive
# run derive on every file that has derived instances in it.
# generated boilerplate code for instances of Binary.
.PHONY: derive
derive:
	@for f in `grep -l OPTIONS_DERIVE -r --include=*.hs .`; do \
            echo derive $$f; derive $$f; done

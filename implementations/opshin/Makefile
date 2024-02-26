OPSHIN_FLAGS := --constant-folding
SRC_DIR := src

build/%: $(SRC_DIR)/%.py
	opshin build spending $^ $(OPSHIN_FLAGS)
	touch $@

.DEFAULT_GOAL := all
.PHONY: all
all: build/lp_minting_policy         \
     build/nft_marketplace_validator \
     build/nft_minting_policy        \
     build/pool_validator

.PHONY: install
install: all
	mkdir -p $$out
	for script in build/*/*.cbor ; do                                     \
		outfile=$$out/$$(basename $$(dirname $$script) | tr _ -).bin; \
		xxd -r -p $$script $$outfile ;                                \
	done

.PHONY: clean
clean:
	rm -rf build

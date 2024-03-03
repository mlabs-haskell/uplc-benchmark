OPSHIN_FLAGS := --constant-folding
SRC_DIR := src

build/lp_minting_policy: src/lp_minting_policy.py
	opshin build minting $^ $(OPSHIN_FLAGS)
	touch $@

build/nft_minting_policy: src/nft_minting_policy.py
	opshin build minting $^ $(OPSHIN_FLAGS)
	touch $@

build/nft_marketplace_validator: src/pool_validator.py
	opshin build spending $^ $(OPSHIN_FLAGS)
	touch $@

build/pool_validator: src/pool_validator.py
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

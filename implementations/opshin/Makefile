OPSHIN_FLAGS := --constant-folding

build/lp_minting_policy: src/lp_minting_policy.py
	opshin build minting $^ $(OPSHIN_FLAGS)

build/nft_marketplace_validator: src/nft_marketplace_validator.py
	opshin build spending $^ $(OPSHIN_FLAGS)

build/nft_minting_policy: src/nft_minting_policy.py
	opshin build minting $^ $(OPSHIN_FLAGS)

build/pool_validator: src/pool_validator.py
	opshin build spending $^ $(OPSHIN_FLAGS)

.DEFAULT_GOAL := all
.PHONY: all
all: build/lp_minting_policy build/nft_marketplace_validator build/nft_minting_policy build/pool_validator

.PHONY: install
install: all
	mkdir -p $$out
	for script in build/*/*.cbor ; do \
		cp -v $$script $$out/$$(basename $$(dirname $$script)).bin ; \
	done


.PHONY: clean
clean:
	rm -rf build

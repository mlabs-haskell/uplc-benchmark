OPSHIN_FLAGS := -fconstant-folding

build/lp_minting_policy: src/lp_minting_policy.py
	rm -rf $@
	opshin build minting $^ $(OPSHIN_FLAGS)

build/nft_minting_policy: src/nft_minting_policy.py
	rm -rf $@
	opshin build minting $^ $(OPSHIN_FLAGS)

build/nft_marketplace_validator: src/nft_marketplace_validator.py
	rm -rf $@
	opshin build spending $^ $(OPSHIN_FLAGS)

build/pool_validator: src/pool_validator.py
	rm -rf $@
	opshin build spending $^ $(OPSHIN_FLAGS)


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

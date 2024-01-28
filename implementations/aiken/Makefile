.PHONY: all
.DEFAULT_GOAL := all
all:
	aiken build
	python3 serialise.py

.PHONY: install
install:
	cp -v *.bin $(PREFIX)

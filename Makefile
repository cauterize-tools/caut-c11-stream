
INSTALL_DIR?=$(HOME)/.local/bin
export PATH := $(INSTALL_DIR):$(PATH)

.PHONY: default clean build test

default: test

clean:
	-@rm -rf .stack-work

build:
	@stack build

test:
	@stack test

install:
	stack setup
	stack build
	mkdir -p $(INSTALL_DIR)
	cp `stack path --dist-dir`/build/caut-c11-stream/caut-c11-stream \
		$(INSTALL_DIR)/caut-c11-stream

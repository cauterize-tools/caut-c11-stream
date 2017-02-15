
INSTALL_DIR?=$(HOME)/.local/bin
export PATH := $(INSTALL_DIR):$(PATH)

ifeq ($(OS),Windows_NT)
	EXECUTABLE=$(shell stack path --dist-dir)\build\caut-c11-stream\caut-c11-stream.exe
	MKDIR=if not exist $(INSTALL_DIR) mkdir $(INSTALL_DIR)
else
	EXECUTABLE=$(shell stack path --dist-dir)/build/caut-c11-stream/caut-c11-stream
	MKDIR=mkdir -p $(INSTALL_DIR)
endif

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
	$(MKDIR)
	cp $(EXECUTABLE) $(INSTALL_DIR)

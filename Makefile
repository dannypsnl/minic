CC := ./build/bin/minic

all:
.PHONY: all

run: build
	@echo "example one ..."
	@$(CC) ./example/hello.m
.PHONY: run

build:
	@echo "building compiler ..."
	@lake build
.PHONY: build

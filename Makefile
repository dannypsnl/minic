CC := ./build/bin/minic

all:
.PHONY: all

run: build
	@echo "example one ..."
	@$(CC) ./example/hello.m
	@echo ""
	@echo "result:"
	@./build/a.out
.PHONY: run

build:
	@echo "building compiler ..."
	@lake build
.PHONY: build

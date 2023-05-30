MC := ./build/bin/minic

all:
.PHONY: all

run: build
	@echo "example one ..."
	@$(MC) ./example/hello.m
	@echo ""
	@echo "result:"
	@./build/a.out
.PHONY: run

build:
	@echo "building compiler ..."
	@lake build
.PHONY: build

clean:
	@rm -r build
.PHONY: clean
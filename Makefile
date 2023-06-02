MC := ./build/bin/minic
SRCS := $(shell find . -type f -not -path '*/lake-packages/*' -name '*.lean')

all:
.PHONY: all

run: build
	@echo "example one ..."
	@$(MC) ./example/hello.m
	@echo ""
	@echo "result:"
	@./build/a.out
.PHONY: run

build: $(SRCS)
	@echo "building compiler ..."
	@lake build
.PHONY: build

clean:
	@rm -r build
.PHONY: clean

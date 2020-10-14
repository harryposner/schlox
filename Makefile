
OBJECT_FILES = build/scanner.o      \
	       build/token.o        \
	       build/ast.o          \
	       build/parser.o       \
	       build/pretty-print.o \
	       build/eval.o         \
	       build/callable.o     \
	       build/schlox.o


.PHONY: all
all: setup $(OBJECT_FILES)
	csc -o build/schlox $(OBJECT_FILES)

.PHONY: setup
setup:
	mkdir -p build

$(OBJECT_FILES): build/%.o: src/%.scm
	csc -c -o $@ $<

.PHONY: clean
clean:
	rm -rf build

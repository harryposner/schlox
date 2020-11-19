SOURCE_DIR = src
SOURCE_FILES = scanner.scm      \
	       token.scm        \
	       ast.scm          \
	       parser.scm       \
	       pretty-print.scm \
	       eval.scm         \
	       callable.scm     \
	       to-string.scm    \
	       schlox.scm

EXECUTABLE = schlox

RELEASE_DIR = build
RELEASE_EXECUTABLE = $(addprefix $(RELEASE_DIR)/, $(EXECUTABLE))
OBJECT_FILES = $(addprefix $(RELEASE_DIR)/, $(SOURCE_FILES:.scm=.o))


DEBUG_DIR = debug
DEBUG_FLAGS = -d3
DEBUG_EXECUTABLE = $(addprefix $(DEBUG_DIR)/, $(EXECUTABLE))
DEBUG_OBJECT_FILES = $(addprefix $(DEBUG_DIR)/, $(SOURCE_FILES:.scm=.o))

.PHONY: default all setup clean remake release debug

default: setup release

all: setup release debug

setup:
	mkdir -p $(RELEASE_DIR) $(DEBUG_DIR)

clean:
	rm -rf $(RELEASE_DIR) $(DEBUG_DIR)

remake: clean all


# Release build rules

release: $(RELEASE_EXECUTABLE)

$(RELEASE_EXECUTABLE): $(OBJECT_FILES)
	csc -o $(addprefix $(RELEASE_DIR)/, $(EXECUTABLE)) $(OBJECT_FILES)

$(RELEASE_DIR)/%.o: $(SOURCE_DIR)/%.scm
	csc -c -o $@ $<


# Debug build rules

debug: $(DEBUG_EXECUTABLE)

$(DEBUG_EXECUTABLE): $(DEBUG_OBJECT_FILES)
	csc $(DEBUG_FLAGS) -o $(DEBUG_EXECUTABLE) $(DEBUG_OBJECT_FILES)

$(DEBUG_DIR)/%.o: $(SOURCE_DIR)/%.scm
	csc -c $(DEBUG_FLAGS) -o $@ $<
